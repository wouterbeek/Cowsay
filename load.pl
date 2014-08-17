% The load file for the cowsay application.

:- multifile(user:project/3).
   user:project(
     Cowsay,
     'A funny cow for communicating with the user.',
     cowsay
   ).

:- use_module(load_project).
:- load_project([
  plc-'Prolog-Library-Collection',
  plDcg
]).

