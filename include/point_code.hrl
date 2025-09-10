%% ANSI 24-bits
%% RFC4666 3.4.1: Affected Point Code
%% ATIS-1000112.3 Figure - Signaling point code encoding
%%  0                   1                   2                   3
%%  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |     Mask      |    Network    |    Cluster    |     Member    |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%                 |MSB-----------------------------------------LSB|
-record(ansi_pc, {mask = 0 :: 0..255,
                  network :: 0..255,
                  cluster :: 0..255,
                  member :: 0..255
                 }).

%% ITU 14-bits
%% RFC4666 3.4.1: Affected Point Code
%%  0                   1                   2                   3
%%  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |      Mask     |0 0 0 0 0 0 0 0 0 0|Zone |     Region    |  SP |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%                                     |MSB---------------------LSB|
-record(itu_pc, {mask = 0 :: 0..255,
                 zone :: 0..7,
                 region :: 0..255,
                 signalling_point :: 0..7
                }).

-type itu_point_code() :: #itu_pc{}.
-type ansi_point_code() :: #ansi_pc{}.
