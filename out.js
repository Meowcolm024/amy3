/* Primitive start */

function print(x) { process.stdout.write(x) }
function println(x) { process.stdout.write(x + "\n") }
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

/* Primitive end */


function what(i){return ((i)<=(0)) ? ({"type": "List","constr": "Nil"}) : ({"type": "List","constr": "Cons","_0":i,"_1":what((i)-(1))})}
function haha(){let l1 = what(5);return println(toString(l1))}

haha()