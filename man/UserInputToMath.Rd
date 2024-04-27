\name{UserInputToMath}
\alias{UserInputToMath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
UserInputToMath: A User Input Basic Arithmetic Calculator
}
\description{
This function takes user input and performs basic arithmetic operations. The following
operations can be performed: Addition, Subtraction, Multiplication, Division, and Modulus. The function takes two numbers or sets of numbers and performs what the user has chosen. If the user chooses to perform an operation from two sets of numbers, then the sets must be of equal length. When a user chooses to enter sets of numbers, it is important to note that the arithmetic operation will be done from the sum of each set of numbers.

Example: 10, 12, 11 + 1, 4, 5

This user input has two sets of vectors of length 3, this will be calculated as the following:

33 + 10

When it comes to using this function, the user needs to be aware of the following:

The user can enter values in one of two ways:

Each number can be spaced in a single line: 13 14 15

Or it can be one value per line:

13

14

15

When the user has entered the desired values, the enter button must be hit twice. The user will know that this was successful if the console shows the message "Read x items". x is referring to the amount of values entered. Once both sets of values have been entered, the user will be asked for the desired operation. The user can input the desired operation in a variety of ways, such as:

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
UserInputToMath()
}
\arguments{
No arguments. User Input needed.
}
\value{
character string in the form "You chose (operation chosen by user), the result is (result of operation)" as well as the numeric result.

If the function was assigned a variable by the user, then the list will be stored there.
}
\author{
Kevin Vasquez <kevinvasquez0718@gmail.com>
}
\seealso{
VectorInputToMath
}
