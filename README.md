RSMonkey is a rust implementation of the monkey language, as outlined in the [interpreter book, by Thornsten Ball](https://interpreterbook.com/).

Example Monkey snippet:

```
let fnTable = {
  "addOne": fn (x) { x + 1 }
};

let y = fnTable["addOne"](5);
puts(y) // => 6
```
