package Assignment "file Assignment.mo"

public 
type ExpLst = list<Exp>;

uniontype Program "Abstract syntax for the Assignments language"
  record PROGRAM
    ExpLst expLst;
    Exp exp;
  end PROGRAM;

end Program;

public 
uniontype Exp
  record INT
    Integer integer;
  end INT;

  record BINARY
    Exp exp1;
    BinOp binOp2;
    Exp exp3;
  end BINARY;

  record UNARY
    UnOp unOp;
    Exp exp;
  end UNARY;

  record ASSIGN
    Ident ident;
    Exp exp;
  end ASSIGN;

  record IDENT
    Ident ident;
  end IDENT;

end Exp;

public 
uniontype BinOp
  record ADD end ADD;

  record SUB end SUB;

  record MUL end MUL;

  record DIV end DIV;

end BinOp;

public 
uniontype UnOp
  record NEG end NEG;

end UnOp;

public 
type Ident = String;

public 
type Value = Integer "Values stored in environments";

public 
type VarBnd = tuple<Ident,Value> "Bindings and environments";

public 
type Env = list<VarBnd>;


protected function lookup
  input Env inEnv;
  input Ident inIdent;
  output Value outValue;
algorithm 
  outValue:=
  matchcontinue (inEnv,inIdent)
    local
      Ident id2,id;
      Value value;
      Env rest;
    case ((id2,value) :: _,id) "lookup returns the value associated with an identifier.
  If no association is present, lookup will fail."
      equation 
        equality(id = id2); then value;
    case ((id2,_) :: rest,id)
      equation 
        failure(equality(id = id2));
        value = lookup(rest, id); then value;
  end matchcontinue;
end lookup;

/*
function lookup
  input Env inEnv;
  input Ident inIdent;
  output Value outInteger;
algorithm
 outInteger:=
  matchcontinue (inEnv,inIdent)
    local  Ident id2,id;  Value value;  Env rest;
    case ( (id2,value) :: rest, id)
      then  if id ==& id2 then value else lookup(rest,id);
  end matchcontinue;
end lookup;
*/

protected function lookupextend
  input Env inEnv;
  input Ident inIdent;
  output Env outEnv;
  output Value outValue;
algorithm 
  (outEnv,outValue):=
  matchcontinue (inEnv,inIdent)
    local
      Env env;
      Ident id;
      Value value;
    case (env,id)
      equation 
        failure(v = lookup(env, id)); then ((id,0) :: env,0);
    case (env,id)
      equation 
        value = lookup(env, id); then (env,value);
  end matchcontinue;
end lookupextend;

protected function update
  input Env inEnv;
  input Ident inIdent;
  input Value inValue;
  output Env outEnv;
algorithm 
  outEnv:=
  matchcontinue (inEnv,inIdent,inValue)
    local
      Env env;
      Ident id;
      Value value;
    case (env,id,value) then (id,value) :: env; 
  end matchcontinue;
end update;

public function evalprogram
  input Program inProgram;
  output Integer outInteger;
algorithm 
  outInteger:=
  matchcontinue (inProgram)
    local
      ExpLst assignments_1,assignments;
      Env env2;
      Value value;
      Exp exp;
    case (PROGRAM(expLst = assignments,exp = exp))
      equation 
        print("evp1\n");
        assignments_1 = listReverse(assignments);
        print("evp2\n");
        env2 = evals({}, assignments_1);
        print("evp3\n");
        (_,value) = eval(env2, exp);
        print("evp4\n"); then value;
  end matchcontinue;
end evalprogram;

protected function evals
  input Env inEnv;
  input ExpLst inExpLst;
  output Env outEnv;
algorithm 
  outEnv:=
  matchcontinue (inEnv,inExpLst)
    local
      Env e,env2,env3,env;
      Value v;
      Ident s;
      Exp exp;
      ExpLst expl;
    case (e,{}) then e; 
    case (env,exp :: expl)
      equation 
        (env2,v) = eval(env, exp);
        s = intString(v);
        print("v: ");
        print(s);
        print("\n");
        env3 = evals(env2, expl); then env3;
  end matchcontinue;
end evals;

protected function eval
  input Env inEnv;
  input Exp inExp;
  output Env outEnv;
  output Integer outInteger;
algorithm 
  (outEnv,outInteger):=
  matchcontinue (inEnv,inExp)
    local
      Env env,env2,env3,env1;
      Value ival,value,v1,v2,v3;
      Ident s,id;
      Exp exp,e1,e2,e;
      BinOp binop;
      UnOp unop;
    case (env,INT(integer = ival)) then (env,ival); 
    case (env,IDENT(ident = id)) "eval of an identifier node will lookup the identifier and return a
    value if present; otherwise insert a binding to zero, and return zero.
"
      equation 
        (env2,value) = lookupextend(env, id);
        s = intString(value);
        print("lookup: ");
        print(s);
        print("\n"); then (env2,value);
    case (env,ASSIGN(ident = id,exp = exp)) "eval of an assignment node returns the updated environment and
    the assigned value.
"
      equation 
        (env2,value) = eval(env, exp);
        env3 = update(env2, id, value); then (env3,value);
    case (env1,BINARY(exp1 = e1,binOp2 = binop,exp3 = e2)) "eval of an addition node ADDe1,e2), etc. in an environment env"
      equation 
        (env2,v1) = eval(env1, e1);
        (env3,v2) = eval(env2, e2);
        v3 = applyBinop(binop, v1, v2); then (env3,v3);
    case (env1,UNARY(unOp = unop,exp = e))
      equation 
        (env2,v1) = eval(env1, e);
        v2 = applyUnop(unop, v1); then (env2,v2);
  end matchcontinue;
end eval;

protected function applyBinop
  input BinOp inBinOp1;
  input Integer inInteger2;
  input Integer inInteger3;
  output Integer outInteger;
algorithm 
  outInteger:=
  matchcontinue (inBinOp1,inInteger2,inInteger3)
    local Value v1,v2;
    case (ADD(),v1,v2) then v1 + v2; 
    case (SUB(),v1,v2) then v1 - v2; 
    case (MUL(),v1,v2) then v1*v2; 
    case (DIV(),v1,v2) then v1/v2; 
  end matchcontinue;
end applyBinop;

protected function applyUnop
  input UnOp inUnOp;
  input Integer inInteger;
  output Integer outInteger;
algorithm 
  outInteger:=
  matchcontinue (inUnOp,inInteger)
    local Value v;
    case (NEG(),v) then -v; 
  end matchcontinue;
end applyUnop;
end Assignment;

