"""6.009 Lab 10: Snek Interpreter Part 2"""

import sys
sys.setrecursionlimit(5000)

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
    RETURNS A LIST ORDERED FROM TOP TO BOTTOM
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

    
class Pair:
    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

    def dupe(self):
        if self.cdr == 'nil':
            return Pair(self.car,'nil')
        if not isinstance(self.cdr, Pair):
            return Pair(self.car, self.cdr)
        return Pair(self.car,self.cdr.dupe()) 

def car(arg): 
    if type(arg[0]) == Pair:
        return arg[0].car
    else:
        raise SnekEvaluationError("'car' arguement is not a cons pair")


def cdr(arg): 
    if isinstance(arg[0],Pair):
        return arg[0].cdr
    else:
        raise SnekEvaluationError("'cdr' arguement is not a cons pair")


def SnekList(args):
    if not args:
        return 'nil'
    return Pair(args[0], SnekList(args[1:]))

def SnekLen(args):
    if args[0] == 'nil':
        return 0
    if not isinstance(args[0],Pair):
        raise SnekEvaluationError("Trying to call 'length' on something that is not a list")
    return SnekLen([args[0].cdr]) + 1 
    
    
def SnekElem(args):
    l, index = args
    if l == 'nil':
        raise SnekEvaluationError("cannot select element from an empty list")
    pointer = l
    while pointer.cdr != 'nil' and index != 0:
        pointer = pointer.cdr
        if not isinstance(pointer, Pair) and pointer !='nil':
            raise SnekEvaluationError("linked list does not end with nil")
        index -= 1

    if index != 0:
        raise SnekEvaluationError("index out of range")

    return pointer.car
        
def SnekConcat(args):
    
    if any((not isinstance(arg, Pair) and arg !='nil') for arg in args ):
        raise SnekEvaluationError("Can only concat lists") 
    if not args:
        return 'nil'
    
    if len(args) == 1:
        if args[0] != 'nil':
            return args[0].dupe()
        else: 
            return 'nil'

    if args[0] == 'nil':
        return SnekConcat(args[1:])

    copy = args[0].dupe()
    pointer = copy


    while pointer.cdr != 'nil':
        pointer = pointer.cdr
        if not isinstance(pointer, Pair) and pointer !='nil':
            raise SnekEvaluationError("linked list does not end with nil")
    pointer.cdr = SnekConcat(args[1:])
    return copy


def SnekMap(args):
    func, l = args
    if l=='nil':
        return 'nil' 

    if not isinstance(l, Pair):
        raise SnekEvaluationError("map can only take list as input")
    copy = l.dupe()
    pointer = copy
    
    pointer.car = func([pointer.car])
    while pointer.cdr != 'nil':
        pointer = pointer.cdr
        pointer.car = func([pointer.car])
    return copy


def SnekFilter(args):
    func, l = args
    if l =='nil':
        return l

    
    ret = 'nil'
    pointer = l

    while pointer != 'nil':
        if func([pointer.car]) == '#t':
            if ret == 'nil':
                ret = Pair(pointer.car, 'nil')
                ret_pointer = ret
            else:
                ret_pointer.cdr = Pair(pointer.car,'nil')
                ret_pointer = ret_pointer.cdr
        pointer = pointer.cdr
    return ret

def SnekReduce(args):
    func, l, init = args
    
    ret = init
    pointer = l
    while pointer!='nil':
        ret = func([ret, pointer.car])

        pointer = pointer.cdr
    return ret
    

snek_builtins = {
    '+' : sum,
    '-' : lambda args: -args[0] if len(args) == 1 else (args[0] - sum(args[1:])),
    '*' : mul,
    '/' : div,
    '=?': lambda args: '#t' if all(args[0] == arg for arg in args) else '#f',
    '>' : lambda args: '#t' if all(args[i] > args[i+1] for i in range(len(args)-1)) else '#f',
    '>=': lambda args: '#t' if all(args[i] >= args[i+1] for i in range(len(args)-1)) else '#f',
    '<' : lambda args: '#t' if all(args[i] < args[i+1] for i in range(len(args)-1)) else '#f',
    '<=': lambda args: '#t' if all(args[i] <= args[i+1] for i in range(len(args)-1)) else '#f',
    'cons': lambda args: Pair(*args),
    'car': car, 
    'cdr': cdr,
    'list': SnekList,
    'length': SnekLen,
    'elt-at-index': SnekElem,
    'concat': SnekConcat,
    'map': SnekMap,
    'filter': SnekFilter,
    'reduce': SnekReduce,
    'begin' : lambda args: args[-1],
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


def evaluate(tree, space:SnekSpace = SnekSpace(SnekBuiltin())):
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
        if isinstance(tree, int) or isinstance(tree,float) or tree in {'#f', '#t', 'nil'}:
            return tree
        try:
            op = space.lookup(tree)
            return op
        except SnekNameError:
            raise SnekNameError('An element is not a valid function or expression:',tree)
    
    if len(tree) == 0:
        raise SnekEvaluationError("Empty brackets")
    
    
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
    
    if tree[0] == 'and':
        flag = True
        i = 1
        while flag and i< len(tree):
            if (result := evaluate(tree[i],space)) == '#f':
                flag = False
            elif result == '#t':
                i +=1
            else:
                raise SnekEvaluationError("One of 'and' arguements is not a bool")
        
        if flag:
            return '#t'
        return '#f'
        
    if tree[0] == 'or':
        flag = False
        i = 1
        while not flag and i<len(tree):
            if (r := evaluate(tree[i], space)) == '#t':
                flag = True 
            elif r == '#f':
                i+=1
            else:
                raise SnekEvaluationError("One of 'or' arguements is not a bool")
        if flag:
            return '#t'
        return '#f'

    if tree[0] == 'not':
        ret = '#t' if evaluate(tree[1],space) == '#f' else '#f'
        return ret

    if tree[0] == 'if':
        cond, true_exp, false_exp = tree[1:]
        if evaluate(cond,space) == '#t':
            return evaluate(true_exp,space)
        else:
            return evaluate(false_exp,space)
    
    if tree[0] == 'del':
        var = tree[1]
        if var not in space.data:
            raise SnekNameError("'del' is trying to delete a variable that is not bound locally")
        val = space.lookup(var)
        del space.data[var]
        return val

    if tree[0] == 'let':
        var_assign,body = tree[1:]
        let_space = SnekSpace(space)
        for var,val_raw in var_assign:
            val = evaluate(val_raw, space)
            let_space.assign(var,val)
        return evaluate(body,let_space)

    if tree[0] == 'set!':
        var, exp = tree[1:]
        val = evaluate(exp, space)
        pointer = space
        while isinstance(pointer, SnekSpace):
            if var in pointer.data: 
                pointer.assign(var,val)
                return val
            pointer = pointer.parent
        raise SnekNameError("'set!' is trying to change a variable that doesnt exist") 

    # Recursive Case

    temp = [evaluate(i,space) for i in tree]
    try:
        return temp[0](temp[1:])
    except TypeError as err:
        print("Got Python Err:", err)
        raise SnekEvaluationError('First term is not callable:',temp)


def result_and_env(tree, space= None):
    if space is None:
        base = SnekBuiltin()
        space = SnekSpace(base)
    return (evaluate(tree,space),space)


def evaluate_file(code, space = SnekSpace(SnekBuiltin())):
    with open(code,'r') as f:
        s = ''
        for line in f:
            s += line

        token = tokenize(s)
        p = parse(token)
        res = evaluate(p,space)
    return res



if __name__ == '__main__':
    # code in this block will only be executed if lab.py is the main file being
    # run (not when this module is imported)

    # uncommenting the following line will run doctests from above
    doctest.testmod()

    exp = input('in> ')
    
    base = SnekBuiltin()
    globals_ = SnekSpace(base)
    for f in sys.argv[1:]:
        evaluate_file(f,globals_)
    
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



