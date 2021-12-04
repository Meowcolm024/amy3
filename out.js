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


function what(i){return (() => {return (i)<=(0)})() ? (() => {return {"type": "List","constr": "Nil"}})() : (() => {return {"type": "List","constr": "Cons","_0":i,"_1":what((i)-(1))}})()}
function sum(l){return ((__match__) => {if(__match__.constr=="Nil"){return 0}if(__match__.constr=="Cons"){if(true){if(true){let v=__match__._0;let ls=__match__._1;return (v)+(sum(ls))}}}error("Match case not exclusive")})(l)}
function haha(){let l1 = what(4000);println(toString((!(true))||(false)));let s = sum(l1);println(toString(s));error("bye!")}

haha()