import java.lang.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Stack;

enum OP{
    OP_ADD,
    OP_SUB,
    OP_ASSIGN,
    OP_DEF,
    OP_AND,
    OP_OR,
    OP_NOT,
    OP_IF,
    OP_DEL,
    OP_LET,
    OP_PRINT,
    OP_SET,  
}

enum Token{
    INT,
    VAR,
    OP,

}

class Builtins{
    HashMap<String,OP> builtin_op = new HashMap<String,OP>();
    public Builtins(){
        builtin_op.put("+", OP.OP_ADD);
        builtin_op.put("-", OP.OP_SUB);
        builtin_op.put(":=", OP.OP_ASSIGN);
        builtin_op.put("function", OP.OP_DEF);
        builtin_op.put("and", OP.OP_AND);
        builtin_op.put("or", OP.OP_OR);
        builtin_op.put("not", OP.OP_NOT);
        builtin_op.put("if", OP.OP_IF);
        builtin_op.put("del", OP.OP_DEL);
        builtin_op.put("let", OP.OP_LET);
        builtin_op.put("dump", OP.OP_PRINT);
        builtin_op.put("set!", OP.OP_SET);

    }

}

public class snek_interpreter {
        public static void main(String[] args) {
            System.out.println("Hello");
            String program = new String("(- 3 (+ 3 (+ 1 2)))");
            Parse p = new Parse(program);
            Env globals_ = new Env();
            Eval e = new Eval(p, globals_);
            //var root = p.root_expression;
            //var memo = p.memo;


            System.out.print(p.program);
        }
}


class Atom{
    public boolean isString=false;
    public boolean isLong = false;
    public boolean isChar = false;
    public boolean isDouble = false;
    public boolean isVar = false;
    public boolean isMemoed = false;
    public boolean isOp = false;
    
    private long long_val;
    private double double_val;
    private String string_val;
    private char char_val;
    private OP op;
    private int[] pair;

    public long get_lval(){
        if (isLong) return long_val;
        throw new Error("Wrong Atom access");
    }
    public double get_dval(){
        if (isDouble) return double_val;
        throw new Error("Wrong Atom access");
    }
    public String get_sval(){
        if (isString) return string_val;
        throw new Error("Wrong Atom access");
    }
    public char get_cval(){
        if (isChar) return char_val;
        throw new Error("Wrong Atom access");
    }
    public OP get_oval(){
        if (isOp) return op;
        throw new Error("Wrong Atom access");
    }
    public int[] get_pval(){
        if (isMemoed) return pair;
        throw new Error("Wrong Atom access");
    }


    public Atom(){
    }

    public Atom(int[] memo_pair){
        isMemoed = true;
        pair = memo_pair;

    }
    public Atom(long val){
        isLong = true;
        long_val = val;
    }
    public Atom(double val){
        isDouble = true;
        double_val = val;
    }
    public Atom(OP op_){
        isOp = true;
        op = op_;
    }
    public Atom(String val){
        isString = true;
        string_val = val;
    }
    public Atom(String val,boolean isvar){
        isVar = true;
        string_val = val;
    }
    public Atom(char val){
        isChar = true;
        char_val = val;
    }
}

class DataType{
    boolean isInt =false;
    boolean isStr = false;

    String str_val;
    long int_val;
    public DataType(String s){
        str_val = s;
        isStr = true;
    }
    public DataType(long i){
        int_val = i;
        isInt = true;
    }
    public DataType(Integer i){
        int_val = i;
        isInt = true;
    }
}

class Eval{
    String program;
    String[] tokens;
    long complexity;
    int[][] paran_pairs;
    Parse parser;
    public Eval(Parse parser_, Env env){
        parser = parser_;
        System.out.print(evaluate(parser_.root_expression).int_val);
    }

    private DataType evaluate(Atom[] expression){
        if (expression[0].isOp){
            var op = expression[0].get_oval();
            switch (op) {
                case OP_ADD:{
                    long sum = 0;
                    for (Atom atom : expression) {
                        if (atom.isOp) continue;
                        if (atom.isMemoed){
                            sum = sum + evaluate(parser.memo.get(atom.get_pval())).int_val;
                        } else {
                            sum = sum + atom.get_lval();
                        }
                    }
                    return new DataType(sum);
                }
                case OP_SUB:{
                    long sub = expression[1].get_lval();
                    for (int i = 2; i < expression.length; i++) {
                        var atom =expression[i];
                        if (atom.isMemoed){
                            sub = sub - evaluate(parser.memo.get(atom.get_pval())).int_val;
                        } else {
                            sub = sub - atom.get_lval();
                        }
                    }
                    return new DataType(sub);

                }
                    
                default:
                return new DataType(137);
            }
        } else {
            return new DataType(138);
        }

    }

}
class Env{
    public void lookup(){

    }

}

class Parse {
    String program;
    String[] tokens;
    long complexity;
    int[][] paran_pairs;
    HashMap<Integer,Integer> paran_map;
    HashMap<int[],Atom[]> memo;// = new HashMap<int[],int[]>();
    Atom[] root_expression;
    public Parse(String new_program) {
        program = new_program.replace('\n', ' ').replaceAll("[)]", " ) ").replaceAll("[(]", " ( ").replaceAll("[' ']+", " ").stripLeading();
        //removes newlines, whitespaces and prepares for tokenization by spacing out paranthesis
        
        tokens = tokenize();

        long l = 0;
        long r = 0;
        for (int i = 0; i < program.length(); i++) {
            char c = program.charAt(i);
            if (c=='(') {
                l++;
            }else if (c==')') {
                r++;
            }
        }
        if (l==r) {
            complexity = l;
        }else{
            throw new Error("Paranthesis do not match");
        }
        paran_map = new HashMap<Integer,Integer>();
        memo = new HashMap<int[],Atom[]>();

        paran_pairs = cross_reference_pran();
        for (int i = 0; i < paran_pairs.length; i++) {
            paran_map.put(paran_pairs[i][0],paran_pairs[i][1]);
        }
        parse();
    }



    /**
     * Can assume that there are no paranthesis
     * @param atom a string token to turn into an atom
     * @return Atom type, basically a wrapper of processed tokens
     */
    private Atom eval_atom(String atom){
        if (atom.equals("(")||atom.equals(")")) {
            throw new Error("paranthesis in evaluate");
        }
        if (atom.matches("[+-]?[0-9]+")){
            return new Atom( Integer.parseInt(atom));
        }
        if (atom.matches("[+-]?([0-9]+.?[0-9]*|.[0-9]+)")){
            return new Atom( Float.parseFloat(atom));
        }
        var b = new Builtins();
        var op =b.builtin_op.get(atom);
        if (atom.matches("[a-zA-Z_][a-zA-Z_!0-9]*")){
            if (op== null){
                return new Atom(atom,true);
            } else{
                return new Atom(op);

            }
        }
        return new Atom(op);
    }

    private Atom[] rec(int i, int j){
        boolean issimple = true;
        for (int ind = i; ind < j; ind++) {
            if (tokens[ind].equals(")") || tokens[ind].equals("(")){
                issimple = false;
            }
        }

        // No paranthesis
        if (issimple){
            Atom[] ret = new Atom[j-i];
            for (int ind = i; ind < j; ind++) {
                ret[ind - i] = eval_atom(tokens[ind]);
            }
            return ret;
        }
        // Here we use memoization to insert proper atoms. 
        ArrayList<Atom> ret = new ArrayList<Atom>();
        
        int ind = i;
        while (ind < j) {
            if (tokens[ind].equals("(")){
                for (int[] pair : paran_pairs) {
                    if (pair[0] == ind){
                        ret.add(new Atom(pair));  // insert Hash key of the memo
                    }
                }
                int end = paran_map.get(ind);
                ind = end;
            } else {
                ret.add(eval_atom(tokens[ind]));
            }
            ind ++;
        }
        Atom[] a = new Atom[]{};
        return ret.toArray(a);
    }

    private void parse(){
        //for (int[] pair : paran_pairs) {
        //    memo.put(pair, new Atom[]{});
        //}
        Atom[] global = new Atom[]{};
        for (int i = 0; i < paran_pairs.length; i++) {
            int start = paran_pairs[i][0];
            int end = paran_pairs[i][1];
            Atom[] expression = rec(start+1,end);
            memo.put( new int[]{start,end}, expression);
            global = expression;
        }
        root_expression = global;
    }


    private String[] tokenize(){
        String[] tokens = program.split("[' ']+");
        return tokens;
    }

    private int[][] cross_reference_pran(){
        Stack<Integer> stack = new Stack<Integer>();
        int[][] ret = new int[(int) complexity][2];
        int reti = 0;
        int toki = -1;
        for (String token : tokens) {
            toki++;
            switch (token) {
                case "(":{
                    stack.push(toki);
                    break;
                }
                case ")":{
                    int start = stack.pop();
                    ret[reti] = new int[]{start,toki};
                    reti ++;
                    break;
                }
                default:
                    continue;
            }
        }
        return ret;
    }

    
}
