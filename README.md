# tar.pl :- A `tar` creator **and** extractor in ~100 lines of Prolog

Okay fine, it's 130ish lines, but a decent number of them are just entrypoint glue and some small helper predicates.

Let's walk through it together.

# Why
Has this ever happened to you: You need to write a parser for a binary file format, but it's total drudgery to manually write out all the byte reading/shifting stuff and structs, and blah blah. Not only that, but once you've written your parser, you may still need to write an encoder as well, which is double the work.

My claim is that using a declarative, logical language like Prolog solves both of those problems (as long as you stick to the pure subset!) Occasionally I see solutions out there for this problem like Kaitai Struct, QuickBMS, or whatever, but really in my heart I know Prolog beats them all. It's a full blown programming language that just happens to work really well for this kind of thing.

We're going to declaratively define a parser for the `tar` format, and once we're done, our code will not only extract .tar files, but create them as well.

# Explanation

In Prolog, there's a feature called DCGs, or Definite Clause Grammars, which are basically a way to describe formats and parse them. In our case, we're going to be using them to parse the `tar` spec [as found here.](https://wiki.osdev.org/USTAR)

Say we wanted to describe a format where it's 2 of the letter `a`, any number of `b`, then two more `a`s. It's a useless "format" but it will explain how these things work:

```prolog
% An instance of `bs` can be either no b's, or one b, followed by another instance of `bs`.
bs --> [].
bs --> [b], bs.

% Our format is two of `a`, some number of `b`, and then two more `a`.
our_format --> [a, a], bs, [a, a].
```

And we can then query Prolog and see that the list `[a,a,b,a,a]` does indeed match this format:

```prolog
?- phrase(our_format, [a,a,b,a,a]).
true .
```

What's interesting also, is that not only can it validate that the input fits the format, it can also generate new examples for us:

```prolog
?- phrase(our_format, Example).
Example = [a, a, a, a] ;
Example = [a, a, b, a, a] ;
Example = [a, a, b, b, a, a] ;
Example = [a, a, b, b, b, a, a] ;
Example = [a, a, b, b, b, b, a, a] ;
Example = [a, a, b, b, b, b, b, a, a] .
```

Cool. Those are all valid examples of the format. This principle applies for whatever you define, as long as you're using the pure logical subset of Prolog (which we will be doing).

Just for example again, let's say we had a format that's 2 of the letter `a`, any value, and then 2 more `a`s.

```prolog
our_format(Val) --> [a, a, Val, a, a].
```

This time you'll see that we can actually extract out the value:

```prolog
?- phrase(our_format(Val), [a,a,20,a,a]).
Val = 20.
```

And of course it can "encode" the value as well:

```prolog
?- phrase(our_format(20), Encoded).
Encoded = [a, a, 20, a, a].
```

So that's DCGs. Let's look at the `tar` file format spec and start tackling it.

According to this [(not very well written) spec](https://wiki.osdev.org/USTAR), a `tar` file is basically a series of 512-byte blocks: a header that contains some metadata, and then some number of 512-byte blocks that are the file's content, padded if necessary, then more headers if there's more files, and so on. Finally there are 2 blocks of just all zeros to signal the end of the stream. So a file with just "hello" in it will consist of:

1. One header block
2. One 512-byte data block padded with zeros
3. Two empty blocks of all zeros

The header block looks like this:

| Offset | Size | Description                                                |
|:-------|:-----|:-----------------------------------------------------------|
| 0      | 100  | File name                                                  |
| 100    | 8    | File mode                                                  |
| 108    | 8    | Owner's numeric user ID                                    |
| 116    | 8    | Group's numeric user ID                                    |
| 124    | 12   | File size in bytes (octal base)                            |
| 136    | 12   | Last modification time in numeric Unix time format (octal) |
| 148    | 8    | Checksum for header record                                 |
| 156    | 1    | Type flag                                                  |
| 157    | 100  | Name of linked file                                        |
| 257    | 6    | UStar indicator "ustar" then NUL                           |
| 263    | 2    | UStar version "00"                                         |
| 265    | 32   | Owner user name                                            |
| 297    | 32   | Owner group name                                           |
| 329    | 8    | Device major number                                        |
| 337    | 8    | Device minor number                                        |
| 345    | 155  | Filename prefix                                            |

and the spec notes that

> The only trick is, that file size is not stored in binary, rather in an ASCII octal string. For example 1025 is stored as '000000002001'.

Bizarre, but that's the way it is. Let's define the header first, and then move onto whatever else we need.

```prolog
tar_header(header(FileName, FileMode, OwnerId, GroupId, FileSize,
                  ModificationTime, Checksum, TypeFlag, LinkedFileName,
                  OwnerUserName, OwnerGroupName, DeviceMajorNumber,
                  DeviceMinorNumber, FilenamePrefix)) -->
    string(100, FileName),
    u64(FileMode),
    u64(OwnerId),
    u64(GroupId),
    octal_number(FileSize),
    octal_number(ModificationTime),
    u64(Checksum),
    u8(TypeFlag), {TypeFlag = 48}, % normal file
    string(100, LinkedFileName),
    "ustar", [0], "00",
    string(32, OwnerUserName),
    string(32, OwnerGroupName),
    u64(DeviceMajorNumber),
    u64(DeviceMinorNumber),
    string(155, FilenamePrefix),
    padding(12). % Seems to be some extension thing
```

Note that this definition looks very similar to the spec itself-- We're not writing any sort of instructions or logic, it's just a plain description of the format of our data. One thing that's slightly different from our previous examples, is that we're using curly brackets. Whatever is inside of curly brackets is just normal Prolog code (not DCG) so all it's saying here is that the TypeFormat byte will always be 48. We could also have written `u8(48)` but I left it like this for consistency.

The spec also doesn't mention a 12-byte extension that seems to exist in files I created using `bsdtar`. We just treat it as padding and ignore it.

In order to try this out, we need to implement the helper predicates it uses, which are `string//2`, `u8//1`, `u64//1`, `octal_number//1`, and `padding//1`.

```prolog
u8(N) --> [N], {N in 0..255}.

u32(N) --> [B1, B2, B3, B4],
           {N in 0..4294967295,
            [B1, B2, B3, B4] ins 0..255,
            N #= (B4 << 24) + (B3 << 16) + (B2 << 8) + B1}.
u64(N) --> u32(DW1), u32(DW2),
           {N in 0..18446744073709551615,
            N #= (DW2 << 32) + DW1}.
```

These predicates just describe unsigned integers-- that is, they define 1 byte, 4 bytes, and 8 bytes respectively, and just do some bit-shifting to turn the bytes into values that we can work with.

For example, here's u32 doing both encoding and decoding.
```prolog
?- phrase(u32(3131313), Bytes).
Bytes = [177, 199, 47, 0].

?- phrase(u32(N), [177, 199, 47, 0]).
N = 3131313.
```

Strings in the `tar` format are fixed-width, and padded at the end with zeros. So, we need to define a predicate that describes a string of a certain length, and strip off the trailing zeros.

```prolog
string(Len, Str) -->
    {length(Bytes, Len),
     append(Str0, Zeros, Bytes),
     maplist(=(0), Zeros),
     maplist(dif(0), Str0)},
    seq(Bytes), {string_codes(Str, Str0)}.
```

We state the following rules:
- `Bytes` has length `Len`.
- We define two lists, `Str0` and `Zeros` and say that concatenated, they are known as `Bytes`.
- `Zeros` is all zeros.
- `Str0`'s elements are all different than zero.

Then, we just say that this predicate describes `Bytes` with `seq//1`. From these rules, Prolog does the work of splitting `Bytes` into `Str0` and `Zeros`, as we only want the actual string content.

```prolog
padding(0) --> [].
padding(Len) --> [0], {Len #> 0, Len_1 #= Len-1}, padding(Len_1).
```

Pretty similar to the `bs` example, except padding just describes some number of zeros. The first rule states that padding of length zero is nothing, and padding with length is one zero, followed by a recursive call for more.

Now for the annoying one-- we need to write a predicate that converts an octal string into a regular number. Why? Because `tar` demands it.
```prolog
octal_string_number_([], 0).
octal_string_number_([H | T], N) :-
    octal_string_number_(T, N0),
    V #= H - 48,
    V in 0..7,
    N #= (N0 * 8) + V.

octal_string_number(Digits, OctalStr, Number) :-
    length(OctalStr, Digits),
    reverse(OctalStr, ROctalStr),
    if_( ROctalStr = [0x20 | _]
         , ROctalStr = [0x20 | ROctalStr2]
         , ROctalStr2 = ROctalStr
       ),
    octal_string_number_(ROctalStr2, Number).

octal_number(N) -->
    {octal_string_number(12, OctalStr, N)},
    seq(OctalStr).
```

The first predicate relates each ASCII code to its numeric value (a number from 0 to 7, because octal is base 8) and then recursively says that each digit is "shifted" right by multiplying by 8.

Here's how you'd implement it in Python:

```python
def convert(s):
    s = list(s)
    n = 0
    while s:
        v = ord(s.pop(0)) - 48
        n = (n * 8) + v
    return out

>> oct(45)
'0o55'
>> convert('55')
45
```

The thing is, these numbers are in little endian, so the least significant byte comes first, and we want to process the numbers starting with the most significant byte. So, we can use a wrapper that just reverses the list first. Also very annoyingly, the `tar` files generated by `bsdtar` (and I think GNU as well) contain random spaces at the end of the numbers which isn't in the spec, so we have to deal with that by tearing off `0x20` if it appears in the list first. That's pretty much all the second predicate is doing.

Finally, we wrap the whole thing in a DCG so we can call it from our existing `tar` header grammar, and that's pretty much it-- we can officially parse `tar` headers.

```prolog
?- phrase_from_file((tar_header(H), ...), "out2.tar", [type(binary)]).
H = header("doc1.txt", 9066805720002608, 9060182779768880, 9060182779768880, 21, 1673997133, 2305900399475831088, 48, "", "", "", 9060182779768880, 9060182779768880, "") .
```

Look at that, `"doc1.txt"`. And here's the cool thing, we can actually encode it back into bytes:

```prolog
?- phrase_from_file((tar_header(H), ...), "out2.tar", [type(binary)]),
   phrase(tar_header(H), Bytes).

H = header("doc1.txt", 9066805720002608, 9060182779768880, 9060182779768880, 21, 1673997133, 2305900399475831088, 48, "", "", "", 9060182779768880, 9060182779768880, ""),
Bytes = [100, 111, 99, 49, 46, 116, 120, 116, 0|...]
```

Now we just need to extract the data and job well done.

First let's define a predicate for describing data blocks. Remember that each block is just 512 bytes:

```prolog
tar_data_block(Data) --> {length(Data, 512)}, seq(Data).
```

We should also define a predicate for describing an arbitrary number of data blocks.

```prolog
tar_data_blocks(0, []) --> [].
tar_data_blocks(N, [H | T]) -->
    {N #> 0, N_1 #= N - 1},
    tar_data_block(H),
    tar_data_blocks(N_1, T).
```

Same as the `padding//1` predicate really, except we're describing `tar` data blocks instead of zeros.

From here, we have everything we need to define a tar "entry"-- or, a single file inside a tar archive.


```prolog
tar_entry(entry(Header, Data)) -->
    tar_header(Header),
    {Header = header(_, _, _, _, FileSize, _, _, _, _, _, _, _, _, _),
     (FileSize mod 512 #\= 0) #<==> M,
     BlocksNeeded #= FileSize div 512 + M},
    tar_data_blocks(BlocksNeeded, Blocks),
    {append(Blocks, Data0),
     length(Data, FileSize),
     append(Data, Zeros, Data0),
     maplist(=(0), Zeros)}.
```

A tar entry consists of:

- A tar header
- Some number of data blocks

So, we say, get the tar header, calculate the number of blocks needed [using this formula](https://blog.bytellect.com/software-development/c/how-to-get-the-ceiling-of-an-integer-division-in-c/), and read that many data blocks using `tar_data_blocks`.

Once it's got the blocks it concatenates them all and removes the padding zeros using `append//2`.

Of course, I'm using "procedural" language here like "get" and "read" but in reality, this code is just "describing" a grammar and can work both ways-- it can encode and decode.

At this point, we can describe an entire `tar` file:

```prolog
tar([]) -->
    padding(1024).
tar([H | T]) -->
    tar_entry(H),
    tar(T).
```

That's it! A `tar` file is any number of `tar_entry` followed by 1024 bytes of `padding`.

I wrote some glue to make it callable from a shell and:

```bash
$ ./tar.pl cf my-cool-tar.tar doc1.txt doc2.txt vorticelli.wav

$ tar tf my-cool-tar.tar
doc1.txt
doc2.txt
vorticelli.wav

$ rm doc1.txt doc2.txt vorticelli.wav

$ ./tar.pl xf my-cool-tar.tar

$ ls
doc1.txt  doc2.txt  my-cool-tar.tar  tar.pl  vorticelli.wav
```

Ain't that just neato?

# PS

It's unfortunately absolutely dog slow. I think it has to do with SWI-Prolog's underlying representation of the linked lists. Even just reading a 200k file into memory causes the runtime to error out, so I had to set a flag that expands the available memory to use-- even just reading the file causes seconds of wait.

I've heard however that Scryer Prolog uses a more efficient internal representation, which might work better but I haven't tried it out yet.

Also, this is a really simple implementation that doesn't deal with file paths, permissions, user IDs or any of that junk.
