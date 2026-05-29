import test from "ava";

import { Evaluator } from "../index.js";

const evaluator = new Evaluator();

test("evaluate", (t) => {
  t.is(evaluator.evaluate("2 + 1"), 3);
  t.deepEqual(
    evaluator.evaluate("[1, 'test'] | filter(this | type == 'string') "),
    ["test"],
  );
});

test("evaluateMultiple", (t) => {
  t.deepEqual(evaluator.evaluateMultiple(["2 + 1", "2 + 2"]), [3, 4]);
});

test("evaluate in context", (t) => {
  const now = Date.now();
  t.is(
    evaluator.evaluate("test | map(this.age) | mean", {
      test: [
        { name: "Bob", age: 32 },
        { name: "Alice", age: 45 },
      ],
    }),
    38.5,
  );
  console.log(`Evaluation took ${Date.now() - now} ms`);
});

test("$now returns a unix timestamp", (t) => {
  const result = evaluator.evaluate("$now");
  t.is(typeof result, "number");
  // Must be after 2024-01-01 (unix 1704067200) and before year 2100 (unix 4102444800)
  t.true(result > 1_704_067_200, `$now returned implausibly small value: ${result}`);
  t.true(result < 4_102_444_800, `$now returned implausibly large value: ${result}`);
});

test("evaluateMultiple in context", (t) => {
  const now = Date.now();
  t.deepEqual(
    evaluator.evaluateMultiple(
      [
        "test | map(this.age) | mean",
        "test | pick('age') | max",
        "test | map(this.age) | mean",
        "test | pick('age') | max",
        "test | map(this.age) | mean",
        "test | pick('age') | max",
        "test | map(this.age) | mean",
        "test | pick('age') | max",
        "test | map(this.age) | mean",
        "test | pick('age') | max",
        "test | map(this.age) | mean",
        "test | pick('age') | max",
        "test | map(this.age) | mean",
        "test | pick('age') | max",
        "test | map(this.age) | mean",
        "test | pick('age') | max",
      ],
      {
        test: [
          { name: "Bob", age: 32 },
          { name: "Alice", age: 45 },
        ],
      },
    ),
    [
      38.5, 45, 38.5, 45, 38.5, 45, 38.5, 45, 38.5, 45, 38.5, 45, 38.5, 45,
      38.5, 45,
    ],
  );
  console.log(`Evaluation took ${Date.now() - now} ms`);
});
