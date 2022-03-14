## Team Members
* Etesam Ansari, ea2905, System Architect
* Nicholas Cheng , njd2135, Language Guru
* Edward Sturt, ecs2220, System Architect
* Elysia Witham, ew2632, Manager and Tester
* Andrey Uspenskiy, avu2106, Language Guru
* Jacob Alexander, jla2206, Tester



# Comma
Welcome to Comma, a programmer-friendly language that simplifies the computation and manipulation of data stored in CSV format!

## 1. Types, Operators & Reserved Words
Comma is primarily designed for the manipulation of matrices, where each element is a double. 

Support for other types is included at the user's risk: namely, a compile-time warning will not be thrown when using unsupported types. Certain special cases -- such as printing char matrices -- are handled separately.
```markdown
Types: double, int, bool, char, list, array, matrix 
Arithmetic operators: + - * / % ^
Comparison operators: < > == <= >= 
Assignment operator: = 
Logic operators: & | !
Reserved words: def, if, eif, else, nul, return, in, for, row, col, true, false
Reserved symbols: {} () [] ; , \ @ ' "

```
## 1.1 Types
```markdown
*Integer (int)*
```
Comma recognizes 32-bit Integers. Their usage, however, is discouraged: under the hood, a given 32-bit Integer is losslessly stored inside the mantissa of a Double. As such, using Doubles directly is recommended.
```markdown
*Double (double)*
```
The standard numerical type in Comma, conforming to the IEEE standard for Double-precision floating point numbers (64-bit). Capable of storing a 32-bit Integer without loss. 
```markdown
*Boolean (bool)*
```
Booleans (and logical operations) are supported by Comma. The reserved words for Booleans are "true" and "false".
```markdown
*Character (char)*
```
A single ASCII character. By default, Comma does not support strings; however, a list or array type may be used to construct a multi-character sequence.
```markdown
*List (list)*
```
A linked list of nodes. Lists in Comma do not include any type checking or safety measures. Also, Lists in comma are not homogenous -- they support mixed data types -- and their length need not be known in advance. The only guarantee Comma provides for Lists is element ordering; users are advised to proceed with caution.
```markdown
*Array (array)*
```
Unlike Lists, Arrays are fixed-length and fixed-type. While the usage of the built-in Matrix type (for 2-D matrices) is recommended for Comma, nested Arrays allow for tensor manipulation, as well as easier access to vectors.
```markdown
*Matrix (matrix)*
```
The standard data type for Comma. Matrices in Comma are simply Arrays of Arrays; however, strict dimensionality checking and an extensive library allow for convenient, optimized operation over Matrix types.

## 1.2 Operators

Operators are characters reserved to execute specific operations. The arithmetic operators are type sensitive, so a double and an int are incompatible arithmetic inputs.
```markdown
+ - * / % ^
```
Arithmetic operators.
```markdown
< > == <= >= 
```
Comparison operators.
```markdown
= 
```
Assignment operator.
```markdown
& | !
```
Logic operators.

## 1.3 Reserved words and symbols

```markdown
*def*
```
Function definitions.
```markdown
*if, eif, else, for*
```
Conditionals and loops.
```markdown
*int, double, bool, char, list, array, matrix*
```
Type declarations.
```markdown
*nul*
```
The Nul value in Comma.
```markdown
*return*
```
Return from function.
```markdown
*in*
```
Special keyword allowing for "enhanced" for-loops over matrices.
```markdown
*row, col*
```
Used to refer to a single row or column in a matrix (for example, inside a loop)
```markdown
*true, false*
```
Boolean values.
```markdown
{} () [] ; , \ ' "
```
Reserved symbols.

```markdown
@ 
```
Lambda expressions.
```markdown
/* */
```
Multiline comment

\newpage

## 2. Statements and Expressions
```markdown
*expr*
  | lvalue = expr; 
  | if ( expr ) { expr }
  | if ( expr ) { expr } else { expr }
  | if ( expr ) { expr } eif { expr } else { expr }
  | for ( expr , expr , expr ) { expr }
  | { expr } for id in matrix_expr
  | lambda_expr
  | id ( param_elements )

*param_elements*
  | lvalue
  | lvalue, param_elements

*array_expr*

  | [ array_elements ]
  | [ array_expr ]
  | [ array_expr, array_expr ]

*array_elements*

  | lvalue 
  | lvalue, array_elements

*matrix_expr*

  | [ [ matrix_elements ], [ matrix_elements ] ]

*matrix_type*

  | lvalue
  | row
  | col

*matrix_elements*

  | double
  | double, matrix_elements

*type_id*

  | int
  | double
  | matrix
  | boolean
  | char

*lvalue*
  | id 
  | lvalue array_expr
```
Comma's lvalues represent stored data that can point to literals, array elements, parameters, and arrays.

### 2.1 Declaration
```markdown
*variable_declaration*

  | type_id id = expr;

*Variable declaration example*
  int x = 5;
```
Variables are declared through a type identifier, an id, an '=' character, and an expression. The expression sits on the right hand side of the '=' character. The expr on the right hand side of the declaration is evaluated and then, if the correct type is returned, assigned to the id.

```markdown
*array_declaration*

  | type_id[] id = type_id[ array_elements ];

*Array declaration example*
  int[] x = int[5,5];
```
Arrays are declared similarly to variables except '[]' is added after the type identifier and before the '[]'. Note that array_elements must be of the same type declared in the array instantiation.

```markdown
*function_declaration*

  | def type_id function_id ( param_elements ) { expr };
  
*Function declaration example*
  def int f ( a, b ) { return a*b; }
```
Functions are declared with the "def" keyword followed by a type identifier, an id, parameters inside of '()', and an expression that is scoped with '{}'. The type identifier specifies the return type. Note that param_elements can be of separate types, unlike array_elements.

### 2.2 Control Flow
```markdown
*expr delimiting with ';'*
  | a = 1; b = 2; c = 3;
```
The ';' character delimits expressions.

```markdown
*if/eif/else*
  | if (a == b) { return 1; }
  | if (a == b) { return 1; } else { return 2; }
  | if (a == b) { return 1; } eif (a == c) { return 2; } else { return 3; }
```
The if keyword must be followed by a boolean evaluated expresion inside of '()' and a '{}' enclosed expr. The if keyword can optionally be followed by the else keyword and a '{}' enclosed statement. The if keyword can also optionally be followed by the eif keyword which must be followed by a boolean evaluated expr inside of '()', a '{}' enclosed expr, and an eif OR an else keyword.

```markdown
*for loop*
  | for ( int i = 1 , i < 10 , i = i + 1 ) { b = b * 2; }
```
When not used in a matrix loop the for keyword must be followed by '()' containing three expr delimited by ',' which must be followed by a '{}' enclosed expr. The first of the three '()' enclosed expr must evaluate to an int and the second must evaluate to a boolean.

```markdown
*matrix for loop*
  | { x = x*2; } for x in my_matrix
```
The for keyword can be used to directly iterate over a matrix type. To use this feature a user must type a '{}' enclosed expression followed by the for keyword, followed by an id, followed by the in keyword, followed by an id. The second id will only be scoped inside of the expr. If the second id is not a matrix type an error will be thrown.

```markdown
*block*
  | { a = 1; }
  | { a = 1; {x = x*2} for x in my_matrix; }
```
A block is defined by the '{}' characters. Variables declared inside of the block have scope within the block. A block inside of another block will allow reuse of ids scoped within the nested block while maintaining the outer variables scope previous to the new declaration and after the nested block ends.

## 2.3 Lambda Expressions
```markdown
*lambda_expr*

  | @type_id ( lvalue ) { expr }
```
Comma will allow lambda expression to be constructed in expressions were typical function declaration would be inconvenient/unnecessary. The beginning of a lambda expression will be designated by the symbol '@' and immediately followed by the type the expression will return and a single variable argument. 
```markdown
*lambda_expr example*

matrix testMatrix = ...
matrix outputMatrix = [ 
                        @double (ele) {
                          return ele + 1;
                        } 
                        for ele in testMatrix]
/* Takes a matrix and adds one to each element */ 

```

\newpage
## 3. Standard Library
```markdown
def nul print ( x : literal )
```
Prints the literal on the standard output where a literal can be a double, bool, int, or char.


```markdown
def matrix parseCSV ( filepath : char[] )
```
Opens a CSV, reads its contents into a matrix type, and returns the matrix.

```markdown
def nul outputCSV ( base_matrix : matrix , filepath : char[] )
```
Opens (or creates if non-existant) a CSV file and exports a matrix's values into a CSV file.

```markdown
def matrix scalarMulti ( scalar : double, base_matrix : matrix)
```
Multiplies the scalar value with every double in the matrix and returns a new matrix.

```markdown
def matrix scalarDiv ( scalar: double, base_matrix : matrix )
```
Divides the scalar value with every double in the matrix and returns a new matrix.

```markdown
def matrix subtractMatrix ( base_matrix : matrix, sub_matrix : matrix )
```
Subtracts the base_matrix by the sub_matrix and returns a result matrix. The base_matrix and the sub_matrix must have the same dimensions or else an error will be thrown.
 ```markdown
 def matrix addMatrix ( base_matrix : matrix, add_matrix : matrix )
 ```
Adds the base_matrix with the sub_matrix and returns a result matrix. The base_matrix and the add_matrix must have the same dimensions or else an error will be thrown.

```markdown
def matrix dotProduct ( base_matrix : matrix, dot_matrix : matrix )
```
Calculates the dot product of base_matrix and dot_matrix and returns the result matrix. The rows of base_matrix and the columns of dot_matrix must have the same length, or else an error will be thrown.
```markdown
def list crossProduct (base_vector : double[] cross_vector : double[])
```
Computes the cross product of two three dimensional vectors and returns the result. 
```markdown
def int length ( my_list : list ) 
```
Returns the number of elements in my_list.

```markdown
def list dimension (matrix : matrix ) 
```
Returns a list [row_length, col_length] where row_length is the number of doubles in a row and col_length is the number of doubles in a column.
```markdown
def double retrieveElement (row_index : int, column_index : int )
```
Returns the value of the element at the specified row_index and column_index.

\newpage

## 4. GCD Example
```markdown
def int gcd(int a, int b){
  if(b == 0){
    return a;
  }else{
    return(b, a%b)
  }
}
```