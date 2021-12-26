#!/usr/bin/env python3
"""6.009 Lab 9: Snek Interpreter"""

import doctest
# NO ADDITIONAL IMPORTS!


###########################
# Snek-related Exceptions #
###########################

class SnekError(Exception):
    """
    A type of exception to be raised if there is an error with a Snek
    program.  Should never be raised directly; rather, subclasses should be
    raised.
    """
    pass


class SnekSyntaxError(SnekError):
    """
    Exception to be raised when trying to evaluate a malformed expression.
    """
    pass


class SnekNameError(SnekError):
    """
    Exception to be raised when looking up a name that has not been defined.
    """
    pass


class SnekEvaluationError(SnekError):
    """
    Exception to be raised if there is an error during evaluation other than a
    SnekNameError.
    """
    pass


############################
# Tokenization and Parsing #
############################


def tokenize(source):
    """
    Splits an input string into meaningful tokens (left parens, right parens,
    other whitespace-separated values).  Returns a list of strings.

    Arguments:
        source (str): a string containing the source code of a Snek
                      expression
    
    """

    # De-comment
    c = source.find(';')
    while c != -1:
        index = c
        while source[index:index+1] != '\n' and index<len(source):
            index +=1
        source = source[:c] + source[index+1:]
        c = source.find(';')
    
    # Remove extra \n:
    k = source.find('\n')
    while k != -1:
        source = source[:k] +' '+ source[k+1:]
        k = source.find('\n')

    # Find all paranthesis
    
    paran = set()
    for i in range(len(source)):
        if source[i] in '()':
            paran.add(i)

    # Convention:
    # i - start of expression, inclusive
    # j - end of expression, exclusive

    i = j = 0
    ret = []
    while j<=len(source):
        if i in paran:
            #paranthesis case
            ret.append(source[i])
            i = i+1
            j = i+1
            continue
        # whitespace case
        if i < len(source) and source[i] == ' ':
            i = j = i+1
            continue
        # end of expression
        if j == len(source) or source[j] in ' ()':
            if i != j:
                ret.append(source[i:j])
            i = j
            j = j + 1
        else:
            j += 1
    return ret        

def pythonize(elem):
    if isinstance(elem,list):
        return elem
    minus_count = 0
    decimal_count = 0
    for i in elem:
        if i not in '-1234567890.':
            return elem
        if i == '-':
            minus_count += 1
        elif i == '.':
            decimal_count += 1
    if minus_count > 1 or decimal_count > 1:
        return elem
    if minus_count == 1 and elem[0] !='-':
        return elem

    elif decimal_count == 1 and len(elem)!=1:
        return float(elem)
    else:
        if elem not in '-.':
            return int(elem)
        else:
            return elem


def paran_pairs(token):
    '''
    returns a list of tuples where: 
        tuple[0] <- index of '('
        tuple[1] <- index of matching ')'
    RETURNS A LIST ORDERED FROM HIGH PRIORITY TO LOW
    '''
    ret = []
    stack = []
    try:
        for i in range(len(token)):
            if token[i] == '(':
                stack.append(i)
            if token[i] == ')':
                opening = stack.pop()
                ret.append((opening,i))
    except IndexError:
        raise SnekSyntaxError("Unmatching paranthesis")
    if len(stack)>0:
        raise SnekSyntaxError("Unmatching paranthesis")
    return ret


def parse(tokens):
    """
    Parses a list of tokens, constructing a repriesentation where:
        * symbols are represented as Python strings
        * numbers are represented as Python ints or floats
        * S-expressions are represented as Python lists

    Arguments:
        tokens (list): a list of strings representing tokens
    """
    if len(tokens)==1 and tokens[0] not in '()':
        return pythonize(tokens[0])
    if '(' not in tokens and ')' not in tokens:
        raise SnekSyntaxError("No paranthesis")
    if tokens.count('(') != tokens.count(')'):
        raise SnekSyntaxError("Unmatching paranthesis")

    paran = paran_pairs(tokens)
    paran.reverse()


    def rec(tokens, memo, offset):
        if '(' not in tokens and ')' not in tokens:
            return [pythonize(i) for i in tokens]
        i = 0
        ret = []
        while i < len(tokens):
            if tokens[i] == '(':
                for pair in paran:
                    if pair[0] == i+offset:
                        j = pair[1] - offset
                        break
                ret.append(memo[(i+offset,j+offset)])
                i = j+1
            else:
                ret.append(pythonize(tokens[i]))
                i += 1
        return ret
    
    memo = {p:[] for p in paran}

    mut_paran = paran[::]
    while mut_paran:
        exp_i = mut_paran.pop()
        pexp = rec(tokens[exp_i[0]+1:exp_i[1]], memo, exp_i[0]+1)    # Using memoization
       

        # ERROR CHECKS FOR SPECIALS DURING PARSING
        if len(pexp)!=0: 
            if pexp[0] == ':=':
                if len(pexp)!=3:
                    raise SnekSyntaxError("Assignment can only take two arguements:",pexp)
                if not( isinstance(pexp[1],str) or isinstance(pexp[1], list)):
                    raise SnekSyntaxError("Invalid variable type in assignment:",pexp)
                if any(not isinstance(_,str) for _ in pexp[1]):
                    raise SnekSyntaxError("Invalid format of variable:",pexp)
                if len(pexp[1]) == 0:
                    raise SnekSyntaxError("Function has no name:",pexp)

            if pexp[0] == 'function':
                if len(pexp)!=3:
                    raise SnekSyntaxError("function definition takes 2 arguements, got:",pexp)
                if not isinstance(pexp[1],list):
                    raise SnekSyntaxError("function arguements must be a list, got:",pexp)
                elif any( not isinstance(_,str) for _ in pexp[1]):
                    raise SnekSyntaxError("function arguements must be strings, got:",pexp)

        memo[exp_i] = pexp
    return pexp



######################
# Built-in Functions #
######################


def mul(args):
    res = 1
    for arg in args:
        res *= arg
    return res

def div(args):
    if not args:
        raise SnekEvaluationError('Division has no arguements:',args)
    if len(args) == 1:
        return 1/args[0]
    
    res = args[0]
    for d in args[1:]:
        res /= d
    return res

    

snek_builtins = {
    '+': sum,
    '-': lambda args: -args[0] if len(args) == 1 else (args[0] - sum(args[1:])),
    '*': mul,
    '/': div,
}
    
###############
# Enviroments #
###############



class SnekEnv:
    def __init__(self):
        self.data = {}
    
class SnekBuiltin(SnekEnv):
    def __init__(self):
        super().__init__()
        self.data = snek_builtins

    def lookup(self, var):
        if var in self.data:
            return self.data[var]
        else:
            raise SnekNameError("Underfined Expression:", var)


class SnekSpace(SnekEnv):
    def __init__(self, parent):
        super().__init__()
        self.parent = parent    

    def lookup(self, var):
        if var in self.data:
            return self.data[var]
        return self.parent.lookup(var)
    
    def assign(self,var,val):
        if all( _ not in var for _ in '( )' ) and any(_ not in '1234567890' for _ in var):
            self.data[var] = val
        else:
            raise SnekNameError("Variable name is invalid:", var)
        return None

##################
# FUNCTION CLASS #
##################


# Currently doesnt support Local variables as FuncDef is a unique class, not an Enviroment.
# Can be easily implemented if required later

class SnekFuncDef():
    def __init__(self,args,body,parent):
        self.parent = parent
        self.args = args
        self.body = body
        
    def __call__(self, vals):
        new_inst = SnekFuncInst(self)
        if len(self.args) != len(vals):
            raise SnekEvaluationError("function has incorrect number of arguements, expected:", self.args,"\n Got:",vals)
        for arg,val in zip(self.args,vals):
            new_inst.data[arg] = val
        return evaluate(self.body,new_inst)
            

class SnekFuncInst(SnekSpace):
    def __init__(self,definition):
        super().__init__(definition.parent)
        self.definition = definition
        
    



##############
# Evaluation #
##############


def evaluate(tree, space = SnekSpace(SnekBuiltin())):
    """
    Evaluate the given syntax tree according to the rules of the Snek
    language.
    Arguments:
        tree (type varies): a fully parsed expression, as the output from the
                            parse function

    >>> evaluate(parse(tokenize('(:= x (+ 2 3))')),SnekSpace(SnekBuiltin()))
    5

    """

    if not isinstance(tree, list):                              # BASE CASE
        if isinstance(tree, int) or isinstance(tree,float):
            return tree
        try:
            op = space.lookup(tree)
            return op
        except SnekNameError:
            raise SnekNameError('An element is not a valid function or expression:',tree)

    # HANDLING SPECIAL TYPES
    
    if tree[0] == ':=':
        if isinstance(tree[1], list):                           # Short Definition 
            new_func = SnekFuncDef(tree[1][1:], tree[2], space)
            space.assign(tree[1][0],new_func)
            return new_func

        exp = evaluate(tree[2],space)
        space.assign(tree[1],exp)
        return exp

    if tree[0] == 'function':
        return SnekFuncDef(tree[1],tree[2], space)

    # Recursive Case

    temp = [evaluate(i,space) for i in tree]
    try:
        return temp[0](temp[1:])
    except TypeError:
        raise SnekEvaluationError('First term is not callable:',temp)


def result_and_env(tree, space= None):
    if space is None:
        base = SnekBuiltin()
        space = SnekSpace(base)
    return (evaluate(tree,space),space)


if __name__ == '__main__':
    # code in this block will only be executed if lab.py is the main file being
    # run (not when this module is imported)

    # uncommenting the following line will run doctests from above
    doctest.testmod()

    exp = input('in> ')
    
    base = SnekBuiltin()
    globals_ = SnekSpace(base)
    while exp != 'QUIT':
        try:
            token = tokenize(exp)
            p = parse(token)
            result = evaluate(p,globals_) 
            print('out> ',result)
        except SnekNameError as err:
            print('SnekNameError',err)
        except SnekEvaluationError as err:
            print('SnekEvaluationError',err)
        except SnekSyntaxError as err:
            print('SnekSyntaxError',err)
        exp = input('in> ') 

    
    pass




