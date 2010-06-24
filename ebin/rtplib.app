%% -*- mode: Erlang; -*-

{application, rtplib,
 [
  {description, "RTP/RTCP codec library"},
  {vsn, "0.1"},
  {id, "rtplib"},
  {modules, [
             rtcp
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {env, []}
 ]
}.
