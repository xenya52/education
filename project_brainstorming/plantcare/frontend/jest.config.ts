// jest.config.ts
import type { Config } from "jest";

const config: Config = {
  preset: "ts-jest",
  testEnvironment: "node",
  moduleFileExtensions: ["ts", "tsx", "js", "jsx", "json", "node"],
  rootDir: "./src",
  testMatch: ["**/__tests__/**/*.(test|spec).ts"],
  transform: {
    "^.+\\.tsx?$": "ts-jest",
  },
  clearMocks: true,
};

export default config;
