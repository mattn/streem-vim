# streem-vim

This is inspired by https://github.com/matz/streem

## Usage

Open new buffer and type following
```
foo
bar
baz
```
And run as below
```
:%Streem {|x| x += " Matz" } | STDOUT
```
You can see below
```
foo Matz
bar Matz
baz Matz
```

## Installation

put streem-vim into your `~/.vim/bundle`.

## License

MIT

## Author

Yasuhiro Matsumoto (a.k.a mattn)
