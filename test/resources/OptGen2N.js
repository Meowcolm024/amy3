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


function getInt(x){return x_0}
function test(){let x_0 = {"type":"MyBase","constr":"Sub0"};
let y_0 = {"type":"MyBase","constr":"Sub1","_0":12};
(() => {return ((15)-(3))==(12)})() ?
 (() => {return {"type":"MyBase","constr":"Sub2","_0":15,"_1":16}})() :
 (() => {return y_0})()
;
let y_1 = {"type":"MyBase","constr":"Sub1","_0":10};
println(toString(({"type":"MyBase","constr":"Sub0"};
("Hello")+("World");
1)+(getInt(15);
(() => {return !((!(12345;
false))||((getInt(0))==(2)))})() ?
 (() => {return getInt(15)})() :
 (() => {return 12})()
;
let tmp_2 = 2;
tmp_2)));
return ((__match__) => {if(__match__.constr=="Sub2"){if(true){if(true){return println("Hello1\n")}}}if(__match__.constr=="Sub3"){if(true){if(true){let any=__match__._0;return ((__match__) => {if(__match__.constr=="Sub2"){if(true){if(true){return println("Hello2\n")}}}if(true){return println("Hello3\n")}error("Match case not exclusive")})(any_3)
}}}if(true){return println("Hello4\n")}error("Match case not exclusive")})({"type":"MyBase","constr":"Sub3","_0":y_1,"_1":y_1})
}

test()