#!/usr/bin/env swipl

:- use_module(library(main)).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).
:- use_module(library(reif)).
:- use_module(library(readutil)).

:- initialization(main, main).

seq([])     --> [].
seq([E|Es]) --> [E], seq(Es).

u8(N) --> [N], {N in 0..255}.

u32(N) --> [B1, B2, B3, B4],
           {N in 0..4294967295,
            [B1, B2, B3, B4] ins 0..255,
            N #= (B4 << 24) + (B3 << 16) + (B2 << 8) + B1}.
u64(N) --> u32(DW1), u32(DW2),
           {N in 0..18446744073709551615,
            N #= (DW2 << 32) + DW1}.

string(Len, Str) -->
    {length(Bytes, Len),
     append(Str0, Zeros, Bytes),
     maplist(=(0), Zeros),
     maplist(dif(0), Str0)},
    seq(Bytes), {string_codes(Str, Str0)}.

padding(0) --> [].
padding(Len) --> [0], {Len #> 0, Len_1 #= Len-1}, padding(Len_1).

dump_entry(entry(header(FileName,_,_,_,_,_,_,_,_,_,_,_,_,_), Data)) :-
    open(FileName,write,Out,[type(binary)]),
    maplist(put_byte(Out), Data),
    close(Out).

dumpb(X, Filename) :-
    open(Filename,write,Out,[type(binary)]),
    maplist(put_byte(Out), X),
    close(Out).

octal_string_number_([], 0).
octal_string_number_([H | T], N) :-
    octal_string_number_(T, N0),
    V #= H - 48,
    V in 0..7,
    N #= (N0 * 8) + V.

octal_string_number(Digits, OctalStr, Number) :-
    length(OctalStr, Digits),
    reverse(OctalStr, ROctalStr),
    ROctalStr = [Ch | _],
    if_( Ch = 0x20
       , ROctalStr = [0x20 | ROctalStr2]
       , ROctalStr2 = ROctalStr
       ),
    octal_string_number_(ROctalStr2, Number).

octal_number(N) -->
    {octal_string_number(12, OctalStr, N)},
    seq(OctalStr).

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

tar_data_block(Data) --> {length(Data, 512)}, seq(Data).
tar_data_blocks(0, []) --> [].
tar_data_blocks(N, [H | T]) -->
    {N #> 0, N_1 #= N - 1},
    tar_data_block(H),
    tar_data_blocks(N_1, T).

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

tar([]) -->
    padding(1024).
tar([H | T]) -->
    tar_entry(H),
    tar(T).

read_file(Filename, entry(Header, Data)) :-
    read_file_to_codes(Filename, Data, [type(binary)]),
    length(Data, FileSize),
    Header0 = header(Filename, 0, 0, 0, FileSize, 0, 0x2020202020202020, 48, "", "", "", 0, 0, ""),
    phrase(tar_header(Header0), Header0Bytes),
    sum_list(Header0Bytes, ChecksumVal),
    octal_string_number(8, ChecksumBytes, ChecksumVal),
    phrase(u64(Checksum), ChecksumBytes),
    Header = header(Filename, 0, 0, 0, FileSize, 0, Checksum, 48, "", "", "", 0, 0, "").

main([cf, Out | Files]) :-
    maplist(read_file, Files, Entries),
    phrase(tar(Entries), Bytes),
    dumpb(Bytes, Out).

main([xf, TarFile]) :-
    phrase_from_file(tar(Entries), TarFile, [type(binary)]),
    maplist(dump_entry, Entries).
