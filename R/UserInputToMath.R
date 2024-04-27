UserInputToMath <- function() {
  print("Please follow the rules that are explained in the RDocumentation file so that the function works as intended.")

  # Input user data for the first set of numbers

  print("Enter a series of numbers: ")
  x = scan()

  # Input user data for second set of numbers

  print("Enter a second series of numbers: ")
  y = scan()

  # First if statement validates the length of both sets of numbers.
  # If both sets do not have the same amount of numbers, an error message will display
  # to the user, asking to retry number entry.

  # It also stores the desired operation for future use.

  if(length(x) == length(y)){
    operation_var <- readline(prompt = "Enter desired operation: ")

    # Second if statement takes the user inputed operation and matches it with the
    # corresponding operation. If an invalid entry was made, an error message will
    # display to the user, asking to try again.

    if(operation_var == "add" | operation_var == "addition" | operation_var == "+"){
      z <- sum(x) + sum(y)
      print(paste0("You chose addition, the result is: ", z))
    }
    else if (operation_var == "subtract" | operation_var == "subtraction" | operation_var == "-"){
      z <- sum(x) - sum(y)
      print(paste0("You chose subtraction, the result is: ", z))
    }
    else if (operation_var == "multiply" | operation_var == "multiplication" | operation_var == "*"){
      z <- sum(x) * sum(y)
      print(paste0("You chose multiplication, the result is: ", z))
    }
    else if (operation_var == "divide" | operation_var == "division" | operation_var == "/"){
      z <- sum(x) / sum(y)
      print(paste0("You chose division, the result is: ", z))
    }
    else if (operation_var == "modulo" | operation_var == "modulus" | operation_var == "%%"){
      z <- sum(x) %% sum(y)
      print(paste0("You chose modulus, the result is: ", z))
    }
    else if (operation_var != c("add, subtract, multiply, divide")){
      print("You did not write one of the operations, try again!")
    }
  }
  else if(length(x) != length(y)){
    print("You did not give two vectors of the same length, try again!")
  }

    # This section of the code allows the return value to be a list that can be
    # accessed by the user if the user assigned a variable when calling the function.

    resultList <- list("firstSetOfNumbers" = x,
                       "secondSetOfNumbers" = y,
                       "sumFirstSet" = sum(x),
                       "sumSecondSet" = sum(y),
                       "OperationPerformed" = operation_var,
                       "result" = z)

  return(resultList)
}
