\name{VectorInputToMath}
\alias{VectorInputToMath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
VectorInputToMath: A Modified Form of the UserInputToMath Function.
}
\description{
This function is a modified version of the UserinputToMath function. This function allows a user to pass their own numerical vectors as arguments to the function. The same rules apply when using this function, which will be explained below.

The following operations can be performed: Addition, Subtraction, Multiplication, Division, and Modulus. The function takes two numbers or sets of numbers and performs what the user has chosen. When a user assigns vectors to the argument of the function, they must be of equal length. Here is an example:

x <- c(10, 12, 11)

y <- c(1, 4, 5)

VectorInputToMath(x,y)

This user input has two sets of vectors of length 3, this will be calculated as the following:

33 + 10

After both sets of vectors have been passed as arguments to the function, the user will be asked for the desired operation. The user can input the desired operation in a variety of ways, such as:

* addition, add, +

* subtraction, subtract, -

* multiplication, multiply, *

* division, divide, /

* modulus, modulo, and two percent signs

After an operation has been chosen, the result of the operation will be shown to the user as follows:

"You chose addition, the result is: 43"

The result will show a prompt that displays the desired operation that the user entered, as well as the result. If the user assigned a variable when calling this function, then the assigned variable will store a list with the following values:

* The first set of values from user input

* The second set of values from user input

* The sum of the first set of numbers

* The sum of the second set of numbers

* The operation performed

* The result of the desired operation

All these variables are stored so that the user can access each operation that the user chose, allowing easy access and reference to the values.
}
\usage{
VectorInputToMath(x,y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,y}{
x and y are numerical vectors that must be of equal length
  }
}
\value{
character string in the form "You chose (operation chosen by user), the result is (result of operation)" as well as the numeric result.

If the function was assigned a variable by the user, then the list will be stored there.
}
\author{
Kevin Vasquez <kevinvasquez0718@gmail.com>
}
\seealso{
UserInputToMath
}
