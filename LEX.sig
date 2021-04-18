(*
    Copyright (c) 2020 David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

signature LEX =
sig
    type lexan
    type sys

    val insymbol: lexan -> unit
    val pushBackSymbol: lexan * sys -> unit
     
    (* insymbol sets sy and id which are exported as "read-only" *)
     
    val sy:     lexan -> sys
    val id:     lexan -> string
    
    val getText: lexan -> string
     
    val initial: (unit -> char option) -> lexan
 
    (* Types that can be shared. *)
    structure Sharing:
    sig
        type lexan      = lexan
        and  sys        = sys
    end


end;

