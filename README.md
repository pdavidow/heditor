**_Rewrite of SimpleEditor in Haskell_ (without using exceptions)**

==================================================

**THE PROBLEM**

**Problem Statement**

In this challenge, you must implement a simple text editor. Initially, your editor contains an empty string, _S_. You must perform _Q_ operations of the following 4 types:

1. append(_W_) - Append string _W_ to the end of _S_.
2. delete(_k_) - Delete the last _k_ characters of _S_.
3. print(_k_) - Print the *k*th character of _S_.
4. undo() - Undo the last (not previously undone) operation of type 1 or 2, reverting _S_ to the state it was in prior to that operation.

**Input Format**

The first line contains an integer, _Q_, denoting the number of operations. 
Each line _i_ of the _Q_ subsequent lines (where 1 <= _i_ <= _Q_) defines an operation to be performed. Each operation starts with a single integer, _t_ (where _t_ is a member of set {1,2,3,4}), denoting a type of operation as defined in the _Problem Statement_ above. If the operation requires an argument, _t_ is followed by its space-separated argument. For example, if _t_ = 1 and _W_ = "abcd", line _i_ will be `1 abcd`.


**Constraints**

* 1 <= _Q_ <= 1000000
* 1 <= k <= _S_ length
* The sum of the lengths of all _W_ in the input <= 1000000
* The sum of _k_ over all delete operations <= 2000000
* All input characters are lowercase English letters.
* It is guaranteed that the sequence of operations given as input is possible to perform.

**Output Format**

Each operation of type 3 must print the *k*th character on a new line.


**Sample Input**
````
8
1 abc
3 3
2 3
1 xy
3 2
4 
4 
3 1
````

**Sample Output**
````
c
y
a
````
