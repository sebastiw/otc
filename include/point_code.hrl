%% ANSI 24-bits
%% RFC4666 3.4.1: Affected Point Code
%% ATIS-1000112.3 Figure - Signaling point code encoding
-record(ansi_pc, {mask = 0 :: 0..255,
                  network :: 0..255,
                  cluster :: 0..255,
                  member :: 0..255
                 }).

%% ITU 14-bits
%% RFC4666 3.4.1: Affected Point Code
-record(itu_pc, {mask = 0 :: 0..255,
                 zone :: 0..7,
                 region :: 0..255,
                 signalling_point :: 0..7
                }).

-type itu_point_code() :: #itu_pc{}.
-type ansi_point_code() :: #ansi_pc{}.
