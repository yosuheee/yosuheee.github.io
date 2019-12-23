import { Vec3 } from "/lib/geometry.js";

export const BAR_STATUS = {
  initial: 0,
  power_undecided: 1,
  power_decided: 2,
  hide: 3,
};

export const DISTANCE_STATUS = {
  hide: 0,
  show: 1,
};

export const WORLD_STATUS = {
  normal: 0,
  animation: 1,
  top: 2,
};

export const Game = {
  bar: {
    status: BAR_STATUS.initial,
    start: new Date().getTime(),
    current: new Date().getTime(),
    power: -1,
  },
  distance: {
    status: DISTANCE_STATUS.hide,
    xz: 0,
  },
  world: {
    status: WORLD_STATUS.normal,
    sky_color: [160 / 255, 216 / 255, 239 / 255],
    positions: [Vec3(0, 0, 0)],
    stage: null,
    qtree: null,
    ball: null,
    land: null,
    wind_power: 0,
    wind_angle: 0,
  },
  hit: {
    angle: 0,
    left_key: false,
    right_key: false,
  },
  top: {
    up_key: false,
    down_key: false,
    up_shift_key: false,
    down_shift_key: false,
  },
  camera: {
    position: Vec3(0, 0, 0),
    up: Vec3(0, 0, 0),
    center: Vec3(0, 0, 0),
  },
};

export const Env = {
  bar: {
    speed: 12,
  },
  ball: {
    radius: 0.04267,
  }
};
