const UINT32_MAX = 0xffffffff;

export const HOSTS = Object.freeze({
  KNOWING: "knowing",
  CLUELESS: "clueless",
});

export const STRATEGIES = Object.freeze({
  STAY: "stay",
  SWITCH: "switch",
});

export const DOOR_COUNTS = Object.freeze([3, 10, 100]);

export function normalizeSeed(value) {
  const seed = Number(value);
  if (!Number.isInteger(seed) || seed < 0 || seed > UINT32_MAX) {
    throw new RangeError(`Seed must be an integer from 0 through ${UINT32_MAX}.`);
  }
  return seed >>> 0;
}

export function createMulberry32(seedValue) {
  let seed = normalizeSeed(seedValue);
  return function random() {
    seed = (seed + 0x6d2b79f5) >>> 0;
    let value = seed;
    value = Math.imul(value ^ (value >>> 15), value | 1);
    value ^= value + Math.imul(value ^ (value >>> 7), value | 61);
    return ((value ^ (value >>> 14)) >>> 0) / 4294967296;
  };
}

export function createStats() {
  return {
    totalGames: 0,
    stayGames: 0,
    stayWins: 0,
    switchGames: 0,
    switchWins: 0,
    voidGames: 0,
  };
}

export function assertStats(stats) {
  const fields = [
    "totalGames",
    "stayGames",
    "stayWins",
    "switchGames",
    "switchWins",
    "voidGames",
  ];

  fields.forEach((field) => {
    if (!Number.isInteger(stats[field]) || stats[field] < 0) {
      throw new Error(`${field} must be a nonnegative integer.`);
    }
  });

  if (stats.stayGames + stats.switchGames !== stats.totalGames) {
    throw new Error(
      `Invariant failed: stayGames (${stats.stayGames}) + switchGames (${stats.switchGames}) must equal totalGames (${stats.totalGames}).`,
    );
  }
  if (stats.stayWins > stats.stayGames) {
    throw new Error("stayWins cannot exceed stayGames.");
  }
  if (stats.switchWins > stats.switchGames) {
    throw new Error("switchWins cannot exceed switchGames.");
  }
  return true;
}

function validateConfiguration(host, doorCount) {
  if (!Object.values(HOSTS).includes(host)) {
    throw new RangeError(`Unknown host behavior: ${host}.`);
  }
  if (!DOOR_COUNTS.includes(doorCount)) {
    throw new RangeError(`Door count must be one of ${DOOR_COUNTS.join(", ")}.`);
  }
}

function randomIndex(random, length) {
  return Math.floor(random() * length);
}

function shuffle(values, random) {
  const result = values.slice();
  for (let index = result.length - 1; index > 0; index -= 1) {
    const swapIndex = randomIndex(random, index + 1);
    [result[index], result[swapIndex]] = [result[swapIndex], result[index]];
  }
  return result;
}

export function assertRound(round) {
  const {
    host,
    doorCount,
    pickedDoor,
    carDoor,
    openedDoors,
    remainingDoor,
    voidRound,
  } = round;
  validateConfiguration(host, doorCount);

  if (pickedDoor < 0 || pickedDoor >= doorCount || carDoor < 0 || carDoor >= doorCount) {
    throw new Error("Picked and car doors must be inside the configured door range.");
  }
  if (openedDoors.length !== doorCount - 2) {
    throw new Error(`Host must open exactly ${doorCount - 2} doors.`);
  }
  if (openedDoors.includes(pickedDoor)) {
    throw new Error("Host opened the player's door.");
  }
  if (new Set(openedDoors).size !== openedDoors.length) {
    throw new Error("Host opened the same door more than once.");
  }
  if (remainingDoor === pickedDoor || openedDoors.includes(remainingDoor)) {
    throw new Error("The remaining door must be unpicked and unopened.");
  }
  if (host === HOSTS.KNOWING && openedDoors.includes(carDoor)) {
    throw new Error("Knowing host opened the car.");
  }
  if (host === HOSTS.KNOWING && voidRound) {
    throw new Error("Knowing-host rounds cannot be void.");
  }
  if (voidRound !== openedDoors.includes(carDoor)) {
    throw new Error("A round is void exactly when the host reveals the car.");
  }
  return true;
}

export function createRound({
  host = HOSTS.KNOWING,
  doorCount = 3,
  random,
  pickedDoor,
} = {}) {
  validateConfiguration(host, doorCount);
  if (typeof random !== "function") {
    throw new TypeError("createRound requires a random-number function.");
  }

  const selectedDoor = pickedDoor ?? randomIndex(random, doorCount);
  if (!Number.isInteger(selectedDoor) || selectedDoor < 0 || selectedDoor >= doorCount) {
    throw new RangeError("pickedDoor must identify a configured door.");
  }

  const carDoor = randomIndex(random, doorCount);
  const unpickedDoors = Array.from({ length: doorCount }, (_, index) => index).filter(
    (door) => door !== selectedDoor,
  );

  let remainingDoor;
  let openedDoors;

  if (host === HOSTS.KNOWING) {
    if (carDoor !== selectedDoor) {
      remainingDoor = carDoor;
    } else {
      remainingDoor = unpickedDoors[randomIndex(random, unpickedDoors.length)];
    }
    openedDoors = unpickedDoors.filter((door) => door !== remainingDoor);
  } else {
    const randomOrder = shuffle(unpickedDoors, random);
    openedDoors = randomOrder.slice(0, doorCount - 2);
    remainingDoor = randomOrder.at(-1);
  }

  const round = {
    host,
    doorCount,
    pickedDoor: selectedDoor,
    carDoor,
    openedDoors,
    remainingDoor,
    voidRound: openedDoors.includes(carDoor),
  };
  assertRound(round);
  return round;
}

export function recordVoid(stats) {
  stats.voidGames += 1;
  assertStats(stats);
}

export function commitChoice(stats, round, strategy) {
  if (round.voidRound) {
    throw new Error("A void round cannot be committed to a strategy.");
  }
  if (!Object.values(STRATEGIES).includes(strategy)) {
    throw new RangeError(`Unknown strategy: ${strategy}.`);
  }

  const finalDoor = strategy === STRATEGIES.STAY ? round.pickedDoor : round.remainingDoor;
  const won = finalDoor === round.carDoor;

  if (strategy === STRATEGIES.STAY) {
    stats.stayGames += 1;
    if (won) stats.stayWins += 1;
  } else {
    stats.switchGames += 1;
    if (won) stats.switchWins += 1;
  }
  stats.totalGames += 1;

  assertStats(stats);
  return { finalDoor, won };
}

export function createSimulation({
  seed = 25335,
  host = HOSTS.KNOWING,
  doorCount = 3,
} = {}) {
  validateConfiguration(host, doorCount);
  return {
    seed: normalizeSeed(seed),
    host,
    doorCount,
    random: createMulberry32(seed),
    stats: createStats(),
  };
}

export function runCountedGames(simulation, strategy, count) {
  if (!Number.isInteger(count) || count < 0) {
    throw new RangeError("Game count must be a nonnegative integer.");
  }

  let counted = 0;
  let attempts = 0;
  while (counted < count) {
    const round = createRound(simulation);
    attempts += 1;
    if (round.voidRound) {
      recordVoid(simulation.stats);
      continue;
    }
    commitChoice(simulation.stats, round, strategy);
    counted += 1;
  }
  return { counted, attempts };
}

export function formatRate(wins, games, digits = 1) {
  if (games === 0) return "— (no games yet)";
  return `${((wins / games) * 100).toFixed(digits)}%`;
}

export function theoreticalRates(host, doorCount) {
  validateConfiguration(host, doorCount);
  if (host === HOSTS.CLUELESS) {
    return { stay: 0.5, switch: 0.5 };
  }
  return { stay: 1 / doorCount, switch: (doorCount - 1) / doorCount };
}

export function classicVariabilityMessage(switchGames) {
  if (switchGames < 10) {
    return "Run at least 10 classic three-door switch games before judging the rate.";
  }
  if (switchGames < 30) {
    return "With 10 switch games, 37% to 96% is ordinary luck.";
  }
  if (switchGames < 100) {
    return "With 30 switch games, 50% to 84% is ordinary luck.";
  }
  if (switchGames < 500) {
    return "With 100 switch games, 57% to 76% is ordinary luck.";
  }
  return "With 500 switch games, 63% to 71% is ordinary luck.";
}
