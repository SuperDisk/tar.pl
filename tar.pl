:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).
:- use_module(library(reif)).

... --> [].
... --> [_], ... .
seq([])     --> [].
seq([E|Es]) --> [E], seq(Es).
skip(N) --> {length(A, N)}, seq(A).

u8(N) --> [N], {N in 0..255}.

u32(N) --> [B1, B2, B3, B4],
           {N in 0..4294967295,
            [B1, B2, B3, B4] ins 0..255,
            N #= (B4 << 24) + (B3 << 16) + (B2 << 8) + B1}.
u64(N) --> u32(DW1), u32(DW2),
           {N in 0..18446744073709551615,
            N #= (DW2 << 32) + DW1}.

string(Len, Str) --> {length(Str, Len)}, seq(Str).
padding(Len) -->
    {length(Padding, Len),
     maplist(=(0), Padding)},
    seq(Padding).

dumpfile(Length, Blocks, FileName) :-
    append(Blocks, Data),
    length(FileData, Length),
    prefix(FileData, Data),
    open(FileName,write,Out,[type(binary)]),
    maplist(put_byte(Out), FileData),
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

octal_string_number(OctalStr, Number) :-
    length(OctalStr, 12),
    reverse(OctalStr, ROctalStr),
    if_( ROctalStr = [0x20 | _]
         , ROctalStr = [0x20 | ROctalStr2]
         , ROctalStr2 = ROctalStr
       ),
    octal_string_number_(ROctalStr2, Number).

octal_number(N) -->
    {octal_string_number(OctalStr, N)},
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

tar_data_block(Data) -->
    {length(Data, 512)},
    seq(Data).

tar_data_blocks(0, []) --> [].
tar_data_blocks(N, [H | T]) -->
    {N_1 #= N - 1},
    tar_data_block(H),
    tar_data_blocks(N_1, T).

tar_entry(entry(Header, Data)) -->
    tar_header(Header),
    {Header = header(_, _, _, _, FileSize, _, _, _, _, _, _, _, _, _),
     (FileSize mod 512 #\= 0) #<==> M,
     BlocksNeeded #= FileSize div 512 + M},
    tar_data_blocks(BlocksNeeded, Data).

tar([]) -->
    padding(1024).
tar([H | T]) -->
    tar_entry(H),
    tar(T).
