from jexl_python import Evaluator

evaluator = Evaluator()

context =  {
    'users': [
        {'name': 'Bob', 'age': 22},
        {'name': 'Alicia', 'age': 23},
        {'name': 'Peter', 'age': 19},
    ]
}

res = evaluator.evaluate("{meanAge: users | pick('age') | mean | toInt, youngest: users | sortByAttribute('age') | first}", context)

print(res)
