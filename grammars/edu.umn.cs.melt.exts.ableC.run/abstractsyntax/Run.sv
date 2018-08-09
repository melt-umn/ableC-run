grammar edu:umn:cs:melt:exts:ableC:run:abstractsyntax;

imports edu:umn:cs:melt:ableC:abstractsyntax:host;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction;
imports edu:umn:cs:melt:ableC:abstractsyntax:construction:parsing;
imports edu:umn:cs:melt:ableC:abstractsyntax:substitution;
imports edu:umn:cs:melt:ableC:abstractsyntax:env;
imports edu:umn:cs:melt:exts:ableC:templating:abstractsyntax as tmp;

imports silver:langutil;
imports silver:langutil:pp;

global builtin :: Location = builtinLoc("edu:umn:cs:melt:exts:ableC:run:abstractsyntax");

-- spawn a function on its own thread 
abstract production run
top::Stmt ::= origFunc::Expr  argList::[Expr]
{
  propagate substituted;
  top.pp = pp"run ${origFunc.pp}()";
  top.functionDefs := actual_forwards.functionDefs;

  local args::Exprs = foldExpr(argList);
  local id::String = toString(genInt()); 
  local argStructName::String = s"_run_arg_${id}_s";
  local funName::String = s"_run_fn_${id}";
  
  local argStructDcl::Decl =
    typeExprDecl(nilAttribute(),
      structTypeExpr(nilQualifier(),
        structDecl(nilAttribute(),
          justName(name(argStructName, location=builtin)),
          argsToStructItems(argList, 0, openScopeEnv(top.env)),
          location=builtin)));
  
  local funDcl::Decl =
    substDecl( 
      [declRefSubstitution("__origFunc__", origFunc),
       exprsSubstitution("__params__", paramsFromStruct(argList, 0))],
      decls(
        parseDecls(s"""
            static void *${funName}(void *_arg_ptr) {
                struct ${argStructName} *_env = 
                    (struct ${argStructName}*)_arg_ptr;
                (__origFunc__)(__params__);
                free(_env);
            }""")));

  local globalDecls::Decls = foldDecl([argStructDcl, funDcl]);
 
  local structVarName::String = s"_run_struct_var_${id}_s";
  local structVarExpr::Expr = parseExpr(s"""${structVarName}""");

  local actual_forwards::Stmt = seqStmt(
      makeArgStruct(argList,argStructName,structVarName),
        exprStmt(
            directCallExpr(name("run_function",location=builtin),
                consExpr(
                    parseExpr(s"""${funName}"""), consExpr(
                        explicitCastExpr(typeName(
                            directTypeExpr(builtinType(nilQualifier(),voidType())), 
                                pointerTypeExpr(nilQualifier(), baseTypeExpr())), 
                            structVarExpr, location=builtin), 
                        nilExpr())), location=builtin)));

  forwards to injectGlobalDeclsStmt (  globalDecls, actual_forwards );
}

-- retrieve a set of function parameters from a struct
function paramsFromStruct
Exprs ::= args::[Expr] count::Integer
{
    local count_s::String = toString(count);
    return consExpr(parseExpr(s"""_env->f${count_s}"""), 
              if length(args) == 1 then nilExpr()
                 else paramsFromStruct(tail(args), count+1));
}

-- returns a list of struct items from the list of arguments passed in
function argsToStructItems
StructItemList ::= args::[Expr] count::Integer e::Decorated Env
{
  local count_s::String = toString(count);
  
  local h::Expr = head(args);
  h.returnType = h.returnType;
  h.env = e;

  return consStructItem(structItem(nilAttribute(), directTypeExpr(h.typerep), 
        consStructDeclarator(
            structField(name(s"f${count_s}",location=head(args).location),
            baseTypeExpr(), nilAttribute()), nilStructDeclarator())),
            if length(args) == 1 then nilStructItem()
            else argsToStructItems(tail(args), count+1, e));
}

-- return a filled in struct of type structName with the given args
function makeArgStruct
Stmt::= args::[Expr] structName::String varName::String
{	                             
    return seqStmt(
        parseStmt(s"""struct ${structName} *${varName} = malloc(sizeof(struct ${structName}));"""),
                         fillArgs(args,varName, 0));
}

-- fill a struct with a set of arguments
function fillArgs
Stmt::= args::[Expr] varName::String count::Integer
{
   local count_s::String = toString(count);
   local headStmt::Stmt = substStmt([declRefSubstitution("__expr__", head(args))],
                    parseStmt(s"""${varName}->f${count_s} = __expr__;"""));

   return if length(args) == 1 then headStmt  
               else seqStmt(headStmt, fillArgs(tail(args),varName, count+1));
}
