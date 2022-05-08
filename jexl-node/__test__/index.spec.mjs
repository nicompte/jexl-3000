import test from 'ava'

import { Evaluator } from '../index.js';

const evaluator = new Evaluator();

test('evaluate', (t) => {
  t.is(evaluator.evaluate("2 + 1"), 3);
});

test('evaluateMultiple', (t) => {
  t.deepEqual(evaluator.evaluateMultiple(['2 + 1', '2 + 2']), [3, 4]);
});

test('evaluate in context', (t) => {
  const now = Date.now();
  t.is(
    evaluator.evaluate(
      "test | map(this.age) | mean",
      { test: [{ name: 'Bob', age: 32 }, { name: 'Alice', age: 45 }] }
    ), 38.5
  );
  console.log(`Evaluation took ${Date.now() - now} ms`);
});

test('evaluateMultiple in context', (t) => {
  const now = Date.now();
  t.deepEqual(
    evaluator.evaluateMultiple(
      ["test | map(this.age) | mean", "test | pick('age') | max",
        "test | map(this.age) | mean", "test | pick('age') | max",
        "test | map(this.age) | mean", "test | pick('age') | max",
        "test | map(this.age) | mean", "test | pick('age') | max",
        "test | map(this.age) | mean", "test | pick('age') | max",
        "test | map(this.age) | mean", "test | pick('age') | max",
        "test | map(this.age) | mean", "test | pick('age') | max",
        "test | map(this.age) | mean", "test | pick('age') | max",
      ],
      { test: [{ name: 'Bob', age: 32 }, { name: 'Alice', age: 45 }] }
    ),
    [38.5, 45,
      38.5, 45,
      38.5, 45,
      38.5, 45,
      38.5, 45,
      38.5, 45,
      38.5, 45,
      38.5, 45]
  );
  console.log(`Evaluation took ${Date.now() - now} ms`);
});