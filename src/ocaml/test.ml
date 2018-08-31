type t1 = {bar : int; baz:string}
 and t2 = {bar: string}
let x:t1 = {bar = 1;baz = "test"} in
let y:t2 = {bar = "foo"} in x.baz;;

