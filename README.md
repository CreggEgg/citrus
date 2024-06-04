<p align="center">
  <img src="./lemon.jpg">
</p>

# Citrus
Citrus is a statically typed loosely functional language with an emphasis on concurrency inspired by languages including Rust, Gleam, Ocaml, Bend, Odin. 
## Examples
```
let main = fn() {
  println("hello world");
}
```
```
type struct = {
	hello: int,
	world: bool
};
type enum = [
	hello,
	world
];
type alias = enum;
let const = 5;
let add = fn(a: int, b: int): int {
	a+b
};
let main = fn() {
	let handle = add(1, 2);  
	println("Hello world");  
	println(5+5);  
	if const < 3 {   
		println("math broke")  
	} else {   
		println("Its fine")  
	};  
	println(@handle); 
};
```
