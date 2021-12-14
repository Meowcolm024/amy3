/* Primitive start */

function print(x) { process.stdout.write(x) }
function println(x) { process.stdout.write(x + "\n") }
function toInt(x) {
    var res = parseInt(x);
      if (isNaN(res)) {
        error("Error: Could not parse int");
      } else {
        return res;
      }
}
const deasync = require('deasync');
const rl = require('readline').createInterface({
  input: process.stdin,
  output: process.stdout
});
let inputLines = [];
rl.on('line', function(answer) {
  inputLines.push(answer);
});
function readLine() {
  deasync.loopWhile(function(){return inputLines.length <= 0;});
  return inputLines.shift();
}
function toString(x) {
    if (typeof x === 'number') {
        return x.toString()
    } else if (typeof x === 'boolean') {
        return x ? "true" : "false"
    } else if (typeof x === 'string') {
        return x
    } else if (typeof x === 'object') {
        var acc = []; var ty = ""; var cs = "";
        for (const [key, value] of Object.entries(x)) {
            if (key == "type") {
                ty = value; continue;
            }
            if (key == "constr") {
                cs = value; continue;
            }
            acc.push(`${toString(value)}`)
        }
        return ty + "." + cs + "(" + acc.join(", ") + ")"
    } else {
        return "()"
    }
}
function error(x) { println(x); process.exit(1) }

/* Primitive end */


function getInt(x_0){return x_0}
function simpLitMatch(){return ((__match__) => {if(1==__match__){return 1}if(true){let any_0=__match__;return (any_0)+(1)}error("Match case not exclusive")})(getInt(12))
}
function simpADTMatch(){let x_0 = (() => {return {"type":"MyBase","constr":"Sub3","_0":{"type":"MyBase","constr":"Sub1","_0":getInt(1)},"_1":{"type":"MyBase","constr":"Sub2","_0":1,"_1":1}}})();
return ((__match__) => {if(__match__.constr=="Sub3"){if(__match__._0.constr=="Sub1"){if(2==__match__._0._0){if(true){return 0}}}}if(true){return 2}error("Match case not exclusive")})(x_0)
}
function simpIf(){return ((_) => {return 1})
(getInt(12))}
function complexExpr(){let x_0 = (() => {let y_0 = (() => {return {"type":"MyBase","constr":"Sub1","_0":12}})();
return {"type":"MyBase","constr":"Sub2","_0":15,"_1":16}})();
let y_1 = (() => {return {"type":"MyBase","constr":"Sub1","_0":10}})();
return ((_) => {return ((__match__) => {if(__match__.constr=="Sub3"){if(true){if(true){let any_3=__match__._0;return ((__match__) => {if(true){return println("Hello3\n")}error("Match case not exclusive")})(any_3)
}}}error("Match case not exclusive")})({"type":"MyBase","constr":"Sub3","_0":y_1,"_1":y_1})
})
(println(toString(((_) => {return 3})
(getInt(15)))))}
function test(){return ((_) => {return ((_) => {return ((_) => {return complexExpr()})
(simpLitMatch())})
(simpADTMatch())})
(simpIf())}

test()