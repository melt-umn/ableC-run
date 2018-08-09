grammar edu:umn:cs:melt:exts:ableC:run:concretesyntax;

imports edu:umn:cs:melt:ableC:concretesyntax;
imports silver:langutil only ast; 

imports edu:umn:cs:melt:exts:ableC:run:abstractsyntax;

marking terminal Run_t 'run' lexer classes {Ckeyword};

concrete production run_function_c
s::Stmt_c ::= 'run'  fn::PostfixExpr_c  '('  args::ArgumentExprList_c  ')'  ';'
{ 
  s.ast = run(fn.ast, args.ast); 
}

