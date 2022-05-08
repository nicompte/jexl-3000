var a = 'a';
//  ^ variable

var b = function () { };
//  ^ variable

module.e = 'e';
//     ^ property

module.f = function () { };
//     ^ property

module.g = async function () { };
//     ^ property

function i() {
  //     ^ function
}

class Person {
  static foo = bar;
  //      ^ property

  getName() {
    // ^ function.method
  }
}

foo(function callback() {
  // ^ keyword
  //         ^ function
})


c();
// <- function

module.e();
//     ^ function.method
