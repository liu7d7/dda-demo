let code = "let a = 3;let b = 4;a + b;for let c = 0; c < 10; c = c + 2 { c; }";

function lex(code) {
  let i = 0, line = 1, col = 1, tokens = [];
  const whitespace = [' ', '\t', '\r'], _a = 'a'.charCodeAt(0), _z = 'z'.charCodeAt(0), _A = 'A'.charCodeAt(0), _Z = 'Z'.charCodeAt(0), _0 = '0'.charCodeAt(0), _9 = '9'.charCodeAt(0);

  function isIdentifierStart(c) {
    return (c >= _a && c <= _z) || (c >= _A && c <= _Z) || c === '_'.charCodeAt(0) || c === '$'.charCodeAt(0);
  }

  function isNumber(c) {
    return (c >= _0 && c <= _9);
  }

  function isIdentifierContinue(c) {
    return isIdentifierStart(c) || isNumber(c);
  }

  while (i < code.length) {
    let c = code.charAt(i);
    if (c === '\n') {
      line++;
      i++;
      continue;
    } else if (whitespace.includes(c)) {
      i++;
      continue;
    } else if (isNumber(c.charCodeAt(0))) {
      // we only support integers
      let begin = i;
      while (isNumber(c.charCodeAt(0))) {
        i++;
        c = code.charAt(i);
      }

      tokens.push({type: "int", value: parseInt(code.substring(begin, i))});
    } else if (isIdentifierStart(c.charCodeAt(0))) {
      let begin = i;
      while (isIdentifierContinue(c.charCodeAt(0))) {
        i++;
        c = code.charAt(i);
      }

      const types = {
        "for": "for",
        "if": "if",
        "else": "else",
        "let": "let"
      };

      let id = code.substring(begin, i);
      tokens.push({type: types[id] ?? "id", value: id});
    } else if (c === '=') {
      i++;
      c = code.charAt(i);
      if (c === '=') {
        i++;
        tokens.push({type: "eqeq"});
      } else {
        tokens.push({type: "eq"});
      }
    } else if (c === '+') {
      i++;
      c = code.charAt(i);
      if (c === '=') {
        i++;
        tokens.push({type: "addeq"});
      } else if (c === '+') {
        i++;
        tokens.push({type: "inc"});
      } else {
        tokens.push({type: "add"});
      }
    } else if (c === '-') {
      i++;
      c = code.charAt(i);
      if (c === '=') {
        i++;
        tokens.push({type: "subeq"});
      } else if (c === '-') {
        i++;
        c = code.charAt(i);
        if (c === '-') {
          // comment;
          while (i < code.length && code.charAt(i) !== '\n') {
            i++;
          }
        } else {
          tokens.push({type: "dec"});
        }
      } else if (c === '>') {
        i++;
        tokens.push({type: "rightarrow"});
      } else {
        tokens.push({type: "sub"});
      }
    } else if (c === '*') {
      i++;
      c = code.charAt(i);
      if (c === '=') {
        i++;
        tokens.push({type: "muleq"});
      } else {
        tokens.push({type: "mul"});
      }
    } else if (c === '/') {
      i++;
      c = code.charAt(i);
      if (c === '=') {
        i++;
        tokens.push({type: "diveq"});
      } else {
        tokens.push({type: "div"});
      }
    } else if (c === '%') {
      i++;
      c = code.charAt(i);
      if (c === '=') {
        i++;
        tokens.push({type: "modeq"});
      } else {
        tokens.push({type: "mod"});
      }
    } else if (c === '(') {
      i++; tokens.push({type: "lpar"});
    } else if (c === ')') {
      i++; tokens.push({type: "rpar"});
    } else if (c === '[') {
      i++; tokens.push({type: "lsquare"});
    } else if (c === ']') {
      i++; tokens.push({type: "rsquare"});
    } else if (c === '{') {
      i++; tokens.push({type: "lcurly"});
    } else if (c === '}') {
      i++; tokens.push({type: "rcurly"});
    } else if (c === ';') {
      i++; tokens.push({type: "semicolon"});
    } else if (c === ',') {
      i++; tokens.push({type: "comma"});
    } else if (c === '|') {
      i++; tokens.push({type: "or"});
    } else if (c === '&') {
      i++; tokens.push({type: "and"});
    } else if (c === '>') {
      i++;
      c = code.charAt(i);
      if (c === '>') {
        i++;
        tokens.push({type: "rsh"});
      } else if (c === '=') {
        i++;
        tokens.push({type: "geq"});
      } else {
        tokens.push({type: "ge"});
      }
    } else if (c === '<') {
      i++;
      c = code.charAt(i);
      if (c === '<') {
        i++;
        tokens.push({type: "lsh"});
      } else if (c === '=') {
        i++;
        tokens.push({type: "leq"});
      } else if (c === '>') {
        i++;
        tokens.push({type: "neq"});
      } else {
        tokens.push({type: "le"});
      }
    } else {
      return "failed to lex! unknown character '" + c + "'";
    }

    tokens[tokens.length - 1].line = line;
  }

  return tokens;
}

function parseAtom(ctx) {
  if (ctx.cur().type === "sub") {
    ctx.adv();
    let expr = parseAtom(ctx);
    if (typeof(expr) === "string") return expr;
    return {type: "neg", expr, begin: expr.begin, end: expr.end};
  }

  if (ctx.cur().type === "lpar") {
    ctx.adv();
    let expr = parseExpr(ctx);
    if (typeof(expr) === "string") return expr;
    if (ctx.cur().type !== 'rpar') return 'expected right paren, got ' + JSON.stringify(ctx.cur());
    ctx.adv();

    return expr;
  }

  if (ctx.cur().type === "int" || ctx.cur().type === "id") {
    let cur = ctx.cur();
    ctx.adv();
    cur.begin = cur.line;
    cur.end = cur.line;
    return cur;
  }

  return "could not handle token! " + JSON.stringify(ctx.cur());
}

function parsePostFix(ctx) {
  let lhs = parseAtom(ctx);
  if (typeof(lhs) === "string") return lhs;

  if (ctx.cur().type === "inc") {
    let a = {type: "inc", lhs, begin: lhs.begin, end: ctx.cur().line};
    ctx.adv();
    return a;
  }

  if (ctx.cur().type === "dec") {
    let a = {type: "dec", lhs, begin: lhs.begin, end: ctx.cur().line};
    ctx.adv();
    return a;
  }

  return lhs;
}

function parseCall(ctx) {
  let func = parsePostFix(ctx);
  if (typeof(func) === "string") return func;

  while (ctx.cur().type === 'lpar') {
    ctx.adv();
    let args = [];
    for (;;) {
      let arg = parseExpr(ctx);
      if (typeof(arg) === "string") return arg;

      args.push(arg);
      if (ctx.cur().type === 'comma') {
        ctx.adv();
      } else {
        break;
      }
    }

    if (ctx.cur().type !== 'rpar') return 'expected right paren, got ' + JSON.stringify(ctx.cur());

    func = {type: "call", func, args, begin: func.begin, end: ctx.cur().line};
    ctx.adv();
  }

  return func;
}

function parseMul(ctx) {
  let lhs = parseCall(ctx);
  if (typeof(lhs) === "string") return lhs;
  while (ctx.cur().type === "mul" || ctx.cur().type === "div" || ctx.cur().type === "mod") {
    let type = ctx.cur().type;
    ctx.adv();
    let rhs = parseCall(ctx);
    if (typeof(rhs) === "string") return rhs;

    lhs = {type, lhs, rhs, begin: lhs.begin, end: rhs.end};
  }

  return lhs;

}

function parseAdd(ctx) {
  let lhs = parseMul(ctx);
  if (typeof(lhs) === "string") return lhs;
  while (ctx.cur().type === "add" || ctx.cur().type === "sub") {
    let type = ctx.cur().type;
    ctx.adv();
    let rhs = parseMul(ctx);
    if (typeof(rhs) === "string") return rhs;

    lhs = {type, lhs, rhs, begin: lhs.begin, end: rhs.end};
  }

  return lhs;
}

function parseComp(ctx) {
  let lhs = parseAdd(ctx);
  if (typeof(lhs) === "string") return lhs;
  while (ctx.cur().type === "ge" || ctx.cur().type === "le" || ctx.cur().type === "geq" || ctx.cur().type === "leq" || ctx.cur().type === "eqeq" || ctx.cur().type === "neq") {
    let type = ctx.cur().type;
    ctx.adv();
    let rhs = parseAdd(ctx);
    if (typeof(rhs) === "string") return rhs;

    lhs = {type, lhs, rhs, begin: lhs.begin, end: rhs.end};
  }

  return lhs;
}

function parseAnd(ctx) {
  let lhs = parseComp(ctx);
  if (typeof(lhs) === "string") return lhs;
  while (ctx.cur().type === "and") {
    ctx.adv();
    let rhs = parseComp(ctx);
    if (typeof(rhs) === "string") return rhs;

    lhs = {type: "and", lhs, rhs, begin: lhs.begin, end: rhs.end};
  }

  return lhs;
}

function parseOr(ctx) {
  let lhs = parseAnd(ctx);
  if (typeof(lhs) === "string") return lhs;
  while (ctx.cur().type === "or") {
    ctx.adv();
    let rhs = parseAnd(ctx);
    if (typeof(rhs) === "string") return rhs;

    lhs = {type: "or", lhs, rhs, begin: lhs.begin, end: rhs.end};
  }

  return lhs;
}

function parseAssign(ctx) {
  let lhs = parseOr(ctx);
  if (typeof(lhs) === "string") return lhs;
  while (ctx.cur().type === "eq" || ctx.cur().type === "addeq" || ctx.cur().type === "subeq" || ctx.cur().type === "muleq" || ctx.cur().type === "diveq" || ctx.cur().type === "modeq") {
    let type = ctx.cur().type;
    ctx.adv();
    let rhs = parseOr(ctx);
    if (typeof(rhs) === "string") return rhs;

    lhs = {type: type, lhs, rhs, begin: lhs.begin, end: rhs.end};
  }

  return lhs; 
}

function parseExpr(ctx) {
  return parseAssign(ctx);
}

function parseFor(ctx) {
  let begin = ctx.cur().line;
  ctx.adv();
  if (ctx.cur().type !== "let") {
    let comp = parseExpr(ctx);
    if (typeof(comp) === "string") return comp;

    if (ctx.cur().type === "semicolon") {
      ctx.adv();
      let inc = parseExpr(ctx);
      if (typeof(inc) === "string") return inc;
      if (ctx.cur().type !== "lcurly") return "expected lcurly, got " + JSON.stringify(ctx.cur());
      ctx.adv();

      let block = parseBlock(ctx);
      if (typeof(block) === "string") return block;
      if (ctx.cur().type !== "rcurly") return "expected rcurly, got " + JSON.stringify(ctx.cur());

      let out = {type: "partialfor", comp, inc, block, begin, end: ctx.cur().line};
      ctx.adv();

      return out;
    }

    if (ctx.cur().type !== "lcurly") return "expected lcurly, got " + JSON.stringify(ctx.cur());
    ctx.adv();
    let block = parseBlock(ctx);
    if (typeof(block) === "string") return block;
    if (ctx.cur().type !== "rcurly") return "expected rcurly, got " + JSON.stringify(ctx.cur());
    let out = {type: "while", comp, block, begin, end: ctx.cur().line};
    ctx.adv();

    return out;
  }

  let init = parseLet(ctx);
  if (typeof(init) === "string") return init;

  let comp = parseExpr(ctx);
  if (typeof(comp) === "string") return comp;
  if (ctx.cur().type !== "semicolon") return "expected semicolon, got " + JSON.stringify(ctx.cur());
  ctx.adv();

  let inc = parseExpr(ctx);
  if (typeof(inc) === "string") return inc;
  if (ctx.cur().type !== "lcurly") return "expected lcurly, got " + JSON.stringify(ctx.cur());
  ctx.adv();

  let block = parseBlock(ctx);
  if (typeof(block) === "string") return block;
  if (ctx.cur().type !== "rcurly") return "expected rcurly, got " + JSON.stringify(ctx.cur());
  let out = {type: "for", init, comp, inc, block, begin, end: ctx.cur().line};
  ctx.adv();

  return out;
}

function parseIf(ctx) {
  ctx.adv();
  let cond = parseExpr(ctx);
  if (typeof(cond) === "string") return cond;

  let body1, body2;

  if (ctx.cur().type === "rightarrow") {
    ctx.adv();
    body1 = parseStmt(ctx);
    if (typeof(body1) === "string") return body1;
  } else if (ctx.cur().type === "lcurly") {
    ctx.adv();
    body1 = parseBlock(ctx);
    if (typeof(body1) === "string") return body1;
    if (ctx.cur().type !== "rcurly") return "expected rcurly, got " + JSON.stringify(ctx.cur());
    ctx.adv();
  } else {
    return "expected lcurly or rightarrow, got " + JSON.stringify(ctx.cur());
  }

  if (ctx.cur().type === "else") {
    ctx.adv();
    
    if (ctx.cur().type === "if") {
      body2 = parseIf(ctx);
      if (typeof(body2) === "string") return body2;
    } else if (ctx.cur().type === "rightarrow") {
      ctx.adv();
      body2 = parseStmt(ctx);
      if (typeof(body2) === "string") return body2;
    } else if (ctx.cur().type === "lcurly") {
      ctx.adv();
      body2 = parseBlock(ctx);
      if (typeof(body2) === "string") return body2;
      if (ctx.cur().type !== "rcurly") return "expected rcurly, got " + JSON.stringify(ctx.cur());
      ctx.adv();
    } else {
      return "expected lcurly or rightarrow, got " + JSON.stringify(ctx.cur());
    }
  }

  return {type: "if", cond, body1, body2, begin: cond.begin, end: body2?.end ?? body1.end};
}

function parseLet(ctx) {
  let begin = ctx.cur().line;
  if (ctx.cur().type !== "let") return "expected let, got " + JSON.stringify(ctx.cur());
  ctx.adv();

  if (ctx.cur().type === "lcurly") {
    ctx.adv();
    let ids = [];
    if (ctx.cur().type !== "id") return "expected id, got " + JSON.stringify(ctx.cur());
    ids.push(ctx.cur().value);
    ctx.adv();
    while (ctx.cur().type === "comma") {
      ctx.adv();
      if (ctx.cur().type !== "id") return "expected id, got " + JSON.stringify(ctx.cur());
      ids.push(ctx.cur().value);
      ctx.adv();
    }

    if (ctx.cur().type !== "rcurly") return "expected rcurly, got " + JSON.stringify(ctx.cur());
    ctx.adv();

    if (ctx.cur().type !== "eq") return "expected =, got " + JSON.stringify(ctx.cur());
    ctx.adv();

    if (ctx.cur().type !== "lcurly") return "expected lcurly, got " + JSON.stringify(ctx.cur());
    ctx.adv();

    let exprs = [];
    let exp = parseExpr(ctx);
    if (typeof(exp) === "string") return exp;
    exprs.push(exp);
    while (ctx.cur().type === "comma") {
      ctx.adv();
      exp = parseExpr(ctx);
      if (typeof(exp) === "string") return exp;
      exprs.push(exp);
    }

    if (ctx.cur().type !== "rcurly") return "expected rcurly, got " + JSON.stringify(ctx.cur());
    ctx.adv();
    
    if (ctx.cur().type !== "semicolon") return "expected semicolon, got " + JSON.stringify(ctx.cur());
    ctx.adv();

    if (exprs.length !== ids.length) return "multilet: incorrect number of values to assign; expected " + ids.length + ", got " + exprs.length;

    return {type: "multilet", ids, exprs, begin, end: exprs[exprs.length - 1].end};
  }

  if (ctx.cur().type !== "id") return "expected id, got " + JSON.stringify(ctx.cur());
  let id = ctx.cur();
  ctx.adv();

  if (ctx.cur().type !== "eq") return "expected =, got " + JSON.stringify(ctx.cur());
  ctx.adv();

  let expr = parseExpr(ctx);
  if (typeof(expr) === "string") return expr;

  if (ctx.cur().type !== "semicolon") return "expected semicolon, got " + JSON.stringify(ctx.cur());
  ctx.adv();

  return {type: "let", id, expr, begin, end: expr.end};
}

function parseStmt(ctx) {
  let expr = undefined;

  if (ctx.cur().type === "let") {
    return parseLet(ctx);
  } else if (ctx.cur().type === "for") {
    return parseFor(ctx);
  } else if (ctx.cur().type === "if") {
    return parseIf(ctx);
  } else {
    expr = parseExpr(ctx);
  }

  if (typeof(expr) === "string") return expr;
  if (ctx.cur().type !== 'semicolon') return "missing semicolon!\n" + JSON.stringify(ctx);
  ctx.adv();

  return {type: "expstmt", expr, begin: expr.begin, end: expr.end};
}

function parseBlock(ctx) {
  let begin = ctx.cur().begin, end;
  let exprs = []
  while (ctx.pos < ctx.tokens.length && ctx.cur().type !== "rcurly") {
    let exp = parseStmt(ctx);
    if (typeof(exp) === "string") return exp;
    end = exp.end;

    exprs.push(exp);
  }

  return {type: "block", value: exprs, begin, end};
}

function parse(tokens) {
  if (typeof(tokens) === "string") return tokens;

  let ctx = {
    pos: 0,
    tokens: tokens,
    cur() {
      if (!this.tokens[this.pos]) return {type: ""};
      return this.tokens[this.pos];
    },
    adv() {
      this.pos++;
    }
  };

  let root = parseBlock(ctx);
  return root;
}

let ___instr_counter = 0;
let PUSH = ___instr_counter++;
let POP = ___instr_counter++;
let VAR_GET = ___instr_counter++;
let VAR_SET = ___instr_counter++;
let ADD = ___instr_counter++;
let SUB = ___instr_counter++;
let MUL = ___instr_counter++;
let DIV = ___instr_counter++;
let MOD = ___instr_counter++;
let LE = ___instr_counter++;
let LEQ = ___instr_counter++;
let GE = ___instr_counter++;
let GEQ = ___instr_counter++;
let EQ = ___instr_counter++;
let NEQ = ___instr_counter++;
let BRANCH_TRUE = ___instr_counter++;
let BRANCH_FALSE = ___instr_counter++;
let SET_PIXEL = ___instr_counter++;
let INC = ___instr_counter++;
let DEC = ___instr_counter++;
let JUMP = ___instr_counter++;
let NEG = ___instr_counter++;

function compileNumber(root, ctx) {
  ctx.code.push(PUSH);
  ctx.lines.push(root.begin);
  ctx.code.push(root.value);
  ctx.lines.push(root.begin);
}

function compileLet(root, ctx) {
  ctx.locals.set(root.id.value, {slot: ctx.locals.size, line: root.begin});
  return compileAny(root.expr, ctx);
}

function compileMultiLet(root, ctx) {
  for (let i = 0; i < root.ids.length; i++) {
    ctx.locals.set(root.ids[i], {slot: ctx.locals.size, line: root.begin});
    let err = compileAny(root.exprs[i], ctx);
    if (err) return err;
  }
}

function compileNeg(root, ctx) {
  let err = compileAny(root.expr, ctx);
  if (err) return err;

  ctx.code.push(NEG);
  ctx.lines.push(root.begin);
}

function compileAssign(root, ctx) {
  if (root.lhs.type != "id") return "expected id, got " + JSON.stringify(root.lhs);
  if (!ctx.locals.has(root.lhs.value)) return "failed to find local named " + root.lhs.value;
  let err = compileAny(root.rhs, ctx);
  if (err) return err;
  ctx.code.push(VAR_SET);
  ctx.lines.push(root.begin);
  ctx.code.push(ctx.locals.get(root.lhs.value).slot);
  ctx.lines.push(root.begin);
}

function compileVarGet(root, ctx) {
  ctx.code.push(VAR_GET);
  ctx.lines.push(root.begin);
  if (!ctx.locals.has(root.value)) return "failed to find local named " + root.value;
  ctx.code.push(ctx.locals.get(root.value).slot);
  ctx.lines.push(root.begin);
}

function compileFor(root, ctx) {
  let forCtx = {
    code: ctx.code,
    lines: ctx.lines,
    locals: new Map(ctx.locals),
    name: "for",
    containing: ctx,
    funcs: [...ctx.funcs]
  };

  if (root.init) {
    let err = compileLet(root.init, forCtx);
    if (err) return err;
  }

  let blockBegin = ctx.code.length;

  err = compileAny(root.comp, forCtx);
  if (err) return err;

  ctx.code.push(BRANCH_FALSE);
  ctx.lines.push(root.comp.begin);
  let patch = ctx.code.length;
  ctx.code.push(0);
  ctx.lines.push(root.comp.begin);

  err = compileAny(root.block, forCtx);
  if (err) return err;

  err = compileAny(root.inc, forCtx);
  if (err) return err;

  ctx.code.push(POP);
  ctx.lines.push(root.inc.begin);

  for (const it of forCtx.locals.keys()) {
    if (ctx.locals.has(it) || (root.init && it === root.init.id.value)) continue;
    ctx.code.push(POP);
    ctx.lines.push(root.end);
  }

  ctx.code.push(JUMP);
  ctx.code.push(blockBegin);
  ctx.lines.push(root.end);
  ctx.lines.push(root.end);

  ctx.code[patch] = ctx.code.length;

  for (const it of forCtx.locals.keys()) {
    if (ctx.locals.has(it)) continue;
    ctx.code.push(POP);
    ctx.lines.push(root.end);
  }
}

function compileWhile(root, ctx) {
  let forCtx = {
    code: ctx.code,
    lines: ctx.lines,
    locals: new Map(ctx.locals),
    name: "while",
    containing: ctx,
    funcs: [...ctx.funcs]
  };

  let blockBegin = ctx.code.length;
  err = compileAny(root.comp, forCtx);
  if (err) return err;

  ctx.code.push(BRANCH_FALSE);
  ctx.lines.push(root.comp.begin);
  let patch = ctx.code.length;
  ctx.code.push(0);
  ctx.lines.push(root.comp.begin);

  err = compileAny(root.block, forCtx);
  if (err) return err;

  for (const it of forCtx.locals.keys()) {
    if (ctx.locals.has(it)) continue;
    console.log(it);
    ctx.code.push(POP);
    ctx.lines.push(root.end);
  }

  ctx.code.push(JUMP);
  ctx.code.push(blockBegin);
  ctx.lines.push(root.end);
  ctx.lines.push(root.end);

  ctx.code[patch] = ctx.code.length;

  for (const it of forCtx.locals.keys()) {
    if (ctx.locals.has(it)) continue;
    console.log(it);
    ctx.code.push(POP);
    ctx.lines.push(root.end);
  }
}

function compileGlobal(root) {
  let ctx = {
    code: [],
    lines: [],
    locals: new Map(),
    name: "global",
    containing: undefined,
    funcs: []
  };

  let err = compileBlock(root, ctx);
  if (err) return err;

  for (const it of ctx.locals.keys()) {
    ctx.code.push(POP);
    ctx.lines.push(root.end);
  }

  return ctx;
}

function compileBlock(root, ctx) {
  for (const it of root.value) {
    let err = compileAny(it, ctx);
    if (err) return err;
  }
}

function compileExpStmt(root, ctx) {
  let err = compileAny(root.expr, ctx);
  if (err) return err;
  ctx.code.push(POP);
  ctx.lines.push(root.begin);
}

function compileCall(root, ctx) {
  if (root.func.type !== "id") return "only support VarGets for functions";
  if (root.func.value !== "set_pixel") return "unknown function name! expected one of {'set_pixel'}";
  if (root.args.length !== 2) return "expected 2 args, got " + root.args.length;

  let err = compileAny(root.args[0], ctx);
  if (err) return err;

  err = compileAny(root.args[1], ctx);
  if (err) return err;

  ctx.code.push(SET_PIXEL);
  ctx.lines.push(root.begin);
}

function compileInc(root, ctx) {
  if (root.lhs.type !== "id") return "cannot increment anything other than a VarGet";

  ctx.code.push(INC);
  ctx.lines.push(root.begin);
  if (!ctx.locals.has(root.lhs.value)) return "failed to find local named " + root.lhs.value;
  ctx.code.push(ctx.locals.get(root.lhs.value).slot);
  ctx.lines.push(root.begin);
}

function compileDec(root, ctx) {
  if (root.lhs.type !== "id") return "cannot increment anything other than a VarGet";

  ctx.code.push(DEC);
  ctx.lines.push(root.begin);
  if (!ctx.locals.has(root.lhs.value)) return "failed to find local named " + root.lhs.value;
  ctx.code.push(ctx.locals.get(root.lhs.value).slot);
  ctx.lines.push(root.begin);
}

function compileIf(root, ctx) {
  let err = compileAny(root.cond, ctx);
  if (err) return err;

  let ifCtx = {
    code: ctx.code,
    lines: ctx.lines,
    locals: new Map(ctx.locals),
    name: "if",
    containing: ctx,
    funcs: [...ctx.funcs]
  };

  ctx.code.push(BRANCH_FALSE);
  ctx.lines.push(root.cond.begin);
  let patch = ctx.code.length;
  ctx.code.push(0);
  ctx.lines.push(root.cond.begin);

  err = compileAny(root.body1, ifCtx);
  if (err) return err;

  for (const it of ifCtx.locals.keys()) {
    if (ctx.locals.has(it)) continue;
    ctx.code.push(POP);
    ctx.lines.push(root.body1.end);
  }

  ctx.code[patch] = ctx.code.length;
  if (root.body2) {
    let elseCtx = {
      code: ctx.code,
      lines: ctx.lines,
      locals: new Map(ctx.locals),
      name: "else",
      containing: ctx,
      funcs: [...ctx.funcs]
    };

    err = compileAny(root.body2, ifCtx);
    if (err) return err;

    for (const it of elseCtx.locals.keys()) {
      if (ctx.locals.has(it)) continue;
      ctx.code.push(POP);
      ctx.lines.push(root.body2.end);
    }
  }
}

function compileAny(root, ctx) {
  switch (root.type) {
    case "int": return compileNumber(root, ctx);
    case "block": return compileBlock(root, ctx);
    case "id": return compileVarGet(root, ctx);
    case "let": return compileLet(root, ctx);
    case "multilet": return compileMultiLet(root, ctx);
    case "eq": return compileAssign(root, ctx);
    case "call": return compileCall(root, ctx);
    case "expstmt": return compileExpStmt(root, ctx);
    case "partialfor":
    case "for": return compileFor(root, ctx);
    case "if": return compileIf(root, ctx);
    case "while": return compileWhile(root, ctx);
    case "inc": return compileInc(root, ctx);
    case "dec": return compileDec(root, ctx);
    case "neg": return compileNeg(root, ctx);
    case "add":
    case "sub":
    case "div":
    case "mul":
    case "mod":
    case "le":
    case "ge":
    case "leq":
    case "geq":
    case "eqeq":
    case "neq": {
      let a = compileAny(root.lhs, ctx), b = compileAny(root.rhs, ctx);
      if (a || b) return a || b;
      ctx.code.push(root.type === "add" ? ADD : root.type === "sub" ? SUB : root.type === "div" ? DIV : root.type === "mul" ? MUL : root.type === "mod" ? MOD : root.type === "le" ? LE : root.type === "leq" ? LEQ : root.type === "ge" ? GE : root.type === "geq" ? GEQ : root.type === "eqeq" ? EQ : NEQ);
      ctx.lines.push(root.begin);
      break;
    }
    default: return "could not compile " + JSON.stringify(root);
  }
}

function compile(root) {
  if (typeof(root) === "string") return root;

  return compileGlobal(root);
}

function errorDisplay(err) {
    document.getElementById("asm-display").innerHTML = `<div style="width: 100%; height: 100%; display: flex; align-items: center; justify-content: center; overflow: wrap; font-size: 1rem; color: darkred"><div style="overflow: wrap">${err}</div></div>`;
}

function disassemble(code, run, cln, delay) {
  if (typeof(code) === "string") {
    errorDisplay(code);
    return code;
  }

  let lns = code.lines;
  code = code.code;

  let i = 0;
  let lines = [];
  while (i < code.length) {
    let j = i;
    switch (code[i]) {
      case POP: lines.push({line: "pop", desc: "pop the top value off the stack"}); i++; break;
      case PUSH: lines.push({line: `ldi\t${code[i+1]}`, desc: `load immediate value ${code[i+1]} onto the stack`}); i += 2; break;
      case VAR_GET: lines.push({line: `ldv\t${code[i+1]}`, desc: `load variable at stack slot ${code[i+1]}`}); i += 2; break;
      case VAR_SET: lines.push({line: `stv\t${code[i+1]}`, desc: `set variable at stack slot ${code[i+1]} to top value`}); i += 2; break;
      case INC: lines.push({line: `inc\t${code[i+1]}`, desc: `increment variable at stack slot ${code[i+1]}`}); i += 2; break;
      case DEC: lines.push({line: `dec\t${code[i+1]}`, desc: `decrement variable at stack slot ${code[i+1]}`}); i += 2; break;
      case JUMP: lines.push({line: `jmp\t${code[i+1]}`, desc: `jump to ${code[i+1]}`}); i += 2; break;
      case ADD: lines.push({line: "add", desc: `add two top values`}); i++; break;
      case SUB: lines.push({line: "sub", desc: "subtract two top values"}); i++; break;
      case MUL: lines.push({line: "mul", desc: "multiply two top values"}); i++; break;
      case DIV: lines.push({line: "div", desc: "divide two top values"}); i++; break;
      case LE: lines.push({line: "lt", desc: "compare less than"}); i++; break;
      case LEQ: lines.push({line: "leq", desc: "compare less than equal"}); i++; break;
      case GE: lines.push({line: "gt", desc: "compare greater than"}); i++; break;
      case GEQ: lines.push({line: "geq", desc: "compare greater than equal"}); i++; break;
      case EQ: lines.push({line: "eq", desc: "compare equal"}); i++; break;
      case NEQ: lines.push({line: "neq", desc: "compare not equal"}); i++; break;
      case MOD: lines.push({line: "mod", desc: "take modulo of two top values"}); i++; break;
      case NEG: lines.push({line: "neg", desc: "negate top value of stack"}); i++; break;
      case BRANCH_TRUE: lines.push({line: `brt\t${code[i+1]}`, desc: `branch to ${code[i+1]} if value at top of stack truthy`}); i += 2; break;
      case BRANCH_FALSE: lines.push({line: `brf\t${code[i+1]}`, desc: `branch to ${code[i+1]} if value at top of stack falsy`}); i += 2; break;
      case SET_PIXEL: lines.push({line: `sp`, desc: `set pixel at coords; x is top-1, y is top`}); i++; break;
      default: return `failed to disasm opcode ${code[i]}`; break;
    }

    lines[lines.length - 1].num = j;
    lines[lines.length - 1].ln = lns[j];
  }

  function leftPad(num) {
    let s = "" + num;
    return "0".repeat(4-s.length) + s;
  }

  function getLineNumber(textArea) {
    return textArea.value.substring(0, textArea.selectionStart).split("\n").length;
  }

  let currentLineNumber = getLineNumber(document.getElementById("code-input"));
  const begin = `<div style="display: flex; flex-direction: row; padding-left: 1rem;">`;
  
  const beginFocus = `<div id="focus-focus" style="display: flex; flex-direction: row; padding-top: 1rem; padding-bottom: 1rem; padding-right: 1rem; box-shadow: 0 var(--shadow-y-offset) var(--shadow-radius) var(--shadow-color);display: flex;  flex-direction: column;gap: 0.5rem;">`;

  const beginFocusRun = `<div class="stack" style="box-shadow: 0 var(--shadow-y-offset) var(--shadow-radius) var(--shadow-color);"><div id="loading-bar" class="loading-bar" style="background-color: #f8fff8; height: 100%; transition: width ${delay * delayMult / 1000}s linear;"></div><div id="focus-focus" style="padding-top: 1rem; padding-bottom: 1rem; padding-right: 1rem; display: flex; flex-direction: row; gap: 0.5rem; background-color: transparent;">`;

  const beginFocus2 = `<div style="display: flex; flex-direction: row; padding-top: 1rem; padding-bottom: 1rem; padding-right: 1rem; box-shadow: 0 var(--shadow-y-offset) var(--shadow-radius) var(--shadow-color);display: flex;flex-direction: column;gap: 0.5rem;">`;

  const end = `</div>`
  let a = "";
  let first = true;

  for (let i = 0; i < lines.length; i++) {
    let it = lines[i];
    let curFocus = currentLineNumber === it.ln;
    let lastFocus = i > 0 && currentLineNumber === lines[i - 1].ln;
    let nextFocus = i < lines.length - 1 && currentLineNumber === lines[i + 1].ln;

    if (run) {
      curFocus = cln === it.num;
      lastFocus = i > 0 && cln === lines[i - 1].num;
      nextFocus = i < lines.length - 1 && cln === lines[i + 1].num;
    }

    if (curFocus && !lastFocus) {
      a += run ? beginFocusRun : first ? beginFocus : beginFocus2;
      first = false;
    }

    a += begin;
    a += `<span style="width: 4rem; min-width: 4rem; color: #7e747c">${leftPad(it.num)}</span><span style="width: 6rem; min-width: 6rem;color: ${curFocus ? "#000" : "#7e747c"}">${it.line}</span>`;
    if (curFocus) a += `<span style="width: 100%; max-width: 100%; color: #7e747c">${it.desc}</span>`;
    a += end;

    if (curFocus && !nextFocus) {
      a += end;
      if (run) a += end;
    }
  }

  document.getElementById("asm-display").innerHTML = a;
  document.getElementById("focus-focus")?.scrollIntoView({behavior: 'instant', block: 'center', container: 'nearest'});
  document.getElementById("loading-bar")?.classList.toggle("loaded");

  return lines;
}

let codeInput = document.getElementById("code-input");
let compileOutput;

codeInput.addEventListener("input", function(ev) {
  compileOutput = compile(parse(lex(codeInput.value)));
});

codeInput.addEventListener("selectionchange", function(ev) {
  if (!compileOutput) compileOutput = compile(parse(lex(codeInput.value)));
  if (compileOutput) disassemble(compileOutput);
});


let stack = new Int32Array(4096);
let delayMult = 200;

const delay = function (ms) { 
  return new Promise(res => setTimeout(res, ms * delayMult));
}

const delays = [
  1,
  1,
  1.5,
  1.5,
  3,
  3,
  35,
  35,
  35,
  3,
  3,
  3,
  3,
  3,
  3,
  0,
  0,
  3,
  1,
  1,
  4,
  1
];

let globalToken = 0;

async function run() {
  if (!compileOutput) {
    compileOutput = compile(parse(lex(codeInput.value)));
  }

  let token = Math.random();
  globalToken = token;
  clearScreen();

  disassemble(compileOutput, true);

  let code = new Int32Array(compileOutput.code);
  let i = 0;
  let sp = -1;
  while (i < code.length) {
    if (globalToken !== token) break;
    let j = i;
    let del = delays[code[i]];
    if (code[i] === BRANCH_TRUE) {
      del = stack[sp] ? 7 : 3;
    } else if (code[i] === BRANCH_FALSE) {
      del = stack[sp] ? 3 : 7;
    }

    disassemble(compileOutput, true, i, del);
    await delay(del);
    switch (code[i++]) {
      case POP: sp--; break;
      case PUSH: stack[sp+1] = code[i]; i++; sp++; break;
      case VAR_GET: stack[sp+1] = stack[code[i]]; i++; sp++; break;
      case VAR_SET: stack[code[i]] = stack[sp]; i++; break;
      case ADD: stack[sp-1] = stack[sp-1] + stack[sp]; sp--; break;
      case SUB: stack[sp-1] = stack[sp-1] - stack[sp]; sp--; break;
      case DIV: stack[sp-1] = Math.floor(stack[sp-1] / stack[sp]); sp--; break;
      case MUL: stack[sp-1] = stack[sp-1] * stack[sp]; sp--; break;
      case MOD: stack[sp-1] = stack[sp-1] % stack[sp]; sp--; break;
      case LE: stack[sp-1] = (stack[sp-1] < stack[sp]) ? 1 : 0; sp--; break;
      case LEQ: stack[sp-1] = (stack[sp-1] <= stack[sp]) ? 1 : 0; sp--; break;
      case GE: stack[sp-1] = (stack[sp-1] > stack[sp]) ? 1 : 0; sp--; break;
      case GEQ: stack[sp-1] = (stack[sp-1] >= stack[sp]) ? 1 : 0; sp--; break;
      case EQ: stack[sp-1] = (stack[sp-1] === stack[sp]) ? 1 : 0; sp--; break;
      case NEQ: stack[sp-1] = (stack[sp-1] !== stack[sp]) ? 1 : 0; sp--; break;
      case BRANCH_TRUE: if (stack[sp--]) { i = code[i]; } else { i++; } break;
      case BRANCH_FALSE: if (!stack[sp--]) { i = code[i]; } else { i++; } break;
      case SET_PIXEL: setPixel(stack[sp-1], stack[sp]); sp-=1; break;
      case INC: stack[sp+1] = stack[code[i]]; stack[code[i]]++; i++; sp++; break;
      case DEC: stack[sp+1] = stack[code[i]]; stack[code[i]]--; i++; sp++; break;
      case JUMP: i = code[i]; break;
      case NEG: stack[sp] = -stack[sp]; break;
      default: return errorDisplay("unknown opcode " + code[i-1] + "; ts is cooked :(");
    }

    if (sp >= 0) console.log(j, stack.slice(0, sp+1));
  }

  disassemble(compileOutput, false);
}

document.getElementById("run").addEventListener("click", async function (ev) {
  if (ev.target.innerHTML.trim() === "run code") {
    ev.target.innerHTML = "stop code";
    await run();
    ev.target.innerHTML = "run code";
  } else {
    ev.target.innerHTML = "run code";
    globalToken = Math.random();
  }
});

function screenResize(ev) {
  let b = document.getElementById("panel");
  let a = document.getElementById("screen");
  if (b.offsetWidth > b.offsetHeight) {
    a.style.width = 'auto';
    a.style.height = '100%';
  } else {
    a.style.width = '100%';
    a.style.height = 'auto';
  }
}

screenResize();

async function clearScreen() {
  let sc = document.getElementById("screen");
  let a = "";
  let w = 10;
  for (let i = -w; i <= w; i++) {
    for (let j = -w; j <= w; j++) {
      a += `<div id="${j}${i}" class="pixel" style="grid-row: ${i+w+1}; grid-col: ${j+w+1}"></div>`
    }
  }

  sc.innerHTML = a;
}

clearScreen();

function setPixel(x, y) {
  let pix = document.getElementById(`${x}${y}`);
  if (!pix) return;
  if (!pix.style.animation) pix.style.animation = '0.5s forwards cubic-bezier(0, 0, 0, 1) pop-in';
}

window.addEventListener("resize", screenResize);

document.getElementById("cycle-count").addEventListener("input", function (ev) {
  delayMult = ev.target.value;
});


