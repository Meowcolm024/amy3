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
function error(x) {throw new Error(x)}

/* Primitive end */

function fun(){print("input something: ");let x = readLine();print("input one more: ");let y = readLine();let p = {"type": "List","constr": "Cons","_0":x,"_1":{"type": "List","constr": "Cons","_0":y,"_1":{"type": "List","constr": "Nil"}}};return println(((head(p))+("\n"))+(toString(tail(p))))}
function getList(){print("Input a number ('x' to finish): ");let x = readLine();return (() => {return (x)==("x")})() ? (() => {return {"type": "List","constr": "Nil"}})() : (() => {let y = toInt(x);return {"type": "List","constr": "Cons","_0":y,"_1":getList()}})()}
function hi(){let a = {"type": "Maybe","constr": "Just","_0":{"type": "Maybe","constr": "Just","_0":12}};let b = {"type": "Maybe","constr": "Just","_0":{"type": "Maybe","constr": "Nothing"}};println(toString((a)==(b)));println(toString(!(("hello")==("hell0"))));println(toString({"type": "Maybe","constr": "Just","_0":true}));fun();let l = getList();println(toString(l));println(toString(mergeSort(l)));println(("double length is ")+(toString(length(append(l,l)))));return println(("sum is ")+(toString(sum(l))))}



function head(l){return ((__match__) => {if(__match__.constr=="Nil"){error("Empty list")}if(__match__.constr=="Cons"){if(true){if(true){let x=__match__._0;return x}}}error("Match case not exclusive")})(l)}
function tail(l){return ((__match__) => {if(__match__.constr=="Nil"){error("Empty list")}if(__match__.constr=="Cons"){if(true){if(true){let xs=__match__._1;return xs}}}error("Match case not exclusive")})(l)}
function length(l){return ((__match__) => {if(__match__.constr=="Nil"){return 0}if(__match__.constr=="Cons"){if(true){if(true){let ls=__match__._1;return (1)+(length(ls))}}}error("Match case not exclusive")})(l)}
function append(l,r){return ((__match__) => {if(__match__.constr=="Nil"){return r}if(__match__.constr=="Cons"){if(true){if(true){let x=__match__._0;let xs=__match__._1;return {"type": "List","constr": "Cons","_0":x,"_1":append(xs,r)}}}}error("Match case not exclusive")})(l)}
function sum(l){return ((__match__) => {if(__match__.constr=="Nil"){return 0}if(__match__.constr=="Cons"){if(true){if(true){let v=__match__._0;let ls=__match__._1;return (v)+(sum(ls))}}}error("Match case not exclusive")})(l)}
function take(l,i){return ((__match__) => {if(__match__.constr=="Nil"){return {"type": "List","constr": "Nil"}}if(__match__.constr=="Cons"){if(true){if(true){let x=__match__._0;let xs=__match__._1;return (() => {return (i)==(0)})() ? (() => {return {"type": "List","constr": "Nil"}})() : (() => {return {"type": "List","constr": "Cons","_0":x,"_1":take(xs,(i)-(1))}})()}}}error("Match case not exclusive")})(l)}
function drop(l,i){return ((__match__) => {if(__match__.constr=="Nil"){return {"type": "List","constr": "Nil"}}if(__match__.constr=="Cons"){if(true){if(true){let x=__match__._0;let xs=__match__._1;return (() => {return (i)==(0)})() ? (() => {return l})() : (() => {return drop(xs,(i)-(1))})()}}}error("Match case not exclusive")})(l)}
function split(l,i){return {"type": "Pair","constr": "Pair","_0":take(l,i),"_1":drop(l,i)}}
function merge(p,q){return ((__match__) => {if(__match__.constr=="Pair"){if(__match__._0.constr=="Nil"){if(true){let ys=__match__._1;return ys}}}if(__match__.constr=="Pair"){if(true){if(__match__._1.constr=="Nil"){let xs=__match__._0;return xs}}}if(__match__.constr=="Pair"){if(__match__._0.constr=="Cons"){if(true){if(true){if(__match__._1.constr=="Cons"){if(true){if(true){let x=__match__._0._0;let xs=__match__._0._1;let y=__match__._1._0;let ys=__match__._1._1;return (() => {return (x)<(y)})() ? (() => {return {"type": "List","constr": "Cons","_0":x,"_1":merge(xs,{"type": "List","constr": "Cons","_0":y,"_1":ys})}})() : (() => {return {"type": "List","constr": "Cons","_0":y,"_1":merge({"type": "List","constr": "Cons","_0":x,"_1":xs},ys)}})()}}}}}}}error("Match case not exclusive")})({"type": "Pair","constr": "Pair","_0":p,"_1":q})}
function mergeSort(l){return ((__match__) => {if(__match__.constr=="Nil"){return {"type": "List","constr": "Nil"}}if(__match__.constr=="Cons"){if(true){if(__match__._1.constr=="Nil"){let x=__match__._0;return l}}}if(true){return ((__match__) => {if(__match__.constr=="Pair"){if(true){if(true){let x=__match__._0;let y=__match__._1;return merge(mergeSort(x),mergeSort(y))}}}error("Match case not exclusive")})(split(l,~~(length(l))/(2)))}error("Match case not exclusive")})(l)}

hi()