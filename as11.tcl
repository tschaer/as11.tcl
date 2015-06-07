#! /opt/local/bin/tclsh

# as11: Assembler for 68HC11
# Workalike for DOS AS11.EXE
# Small fixes to option handling
# 02.08.2013 T Schaer

array set opcodes [ list \
      ABA  { Inherent 1b } \
      ABX  { Inherent 3a } \
      ABY  { Inherent 183a } \
      ASLA { Inherent 48 } \
      ASLB { Inherent 58 } \
      ASLD { Inherent 05 } \
      ASRA { Inherent 47 } \
      ASRB { Inherent 57 } \
      CBA  { Inherent 11 } \
      CLC  { Inherent 0c } \
      CLI  { Inherent 0e } \
      CLRA { Inherent 4f } \
      CLRB { Inherent 5f } \
      CLV  { Inherent 0a } \
      COMA { Inherent 43 } \
      COMB { Inherent 53 } \
      DAA  { Inherent 19 } \
      DECA { Inherent 4a } \
      DECB { Inherent 5a } \
      DES  { Inherent 34 } \
      DEX  { Inherent 09 } \
      DEY  { Inherent 1809 } \
      FDIV { Inherent 03 } \
      IDIV { Inherent 02 } \
      INCA { Inherent 4c } \
      INCB { Inherent 5c } \
      INS  { Inherent 31 } \
      INX  { Inherent 08 } \
      INY  { Inherent 1808 } \
      LSLA { Inherent 48 } \
      LSLB { Inherent 58 } \
      LSLD { Inherent 05 } \
      LSRA { Inherent 44 } \
      LSRB { Inherent 54 } \
      LSRD { Inherent 04 } \
      MUL  { Inherent 3d } \
      NEGA { Inherent 40 } \
      NEGB { Inherent 50 } \
      NOP  { Inherent 01 } \
      PSHA { Inherent 36 } \
      PSHB { Inherent 37 } \
      PSHX { Inherent 3c } \
      PSHY { Inherent 183c } \
      PULA { Inherent 32 } \
      PULB { Inherent 33 } \
      PULX { Inherent 38 } \
      PULY { Inherent 1838 } \
      ROLA { Inherent 49 } \
      ROLB { Inherent 59 } \
      RORA { Inherent 46 } \
      RORB { Inherent 56 } \
      RTI  { Inherent 3b } \
      RTS  { Inherent 39 } \
      SBA  { Inherent 10 } \
      SEC  { Inherent 0d } \
      SEI  { Inherent 0f } \
      SEV  { Inherent 0b } \
      STOP { Inherent cf } \
      SWI  { Inherent 3f } \
      TAB  { Inherent 16 } \
      TAP  { Inherent 06 } \
      TBA  { Inherent 17 } \
      TEST { Inherent 00 } \
      TPA  { Inherent 07 } \
      TSTA { Inherent 4d } \
      TSTB { Inherent 5d } \
      TSX  { Inherent 30 } \
      TSY  { Inherent 1830 } \
      TXS  { Inherent 35 } \
      TYS  { Inherent 1835 } \
      WAI  { Inherent 3e } \
      XGDX { Inherent 8f } \
      XGDY { Inherent 188f } \
      BCC  { Group0 24 } \
      BCS  { Group0 25 } \
      BEQ  { Group0 27 } \
      BGE  { Group0 2c } \
      BGT  { Group0 2e } \
      BHI  { Group0 22 } \
      BHS  { Group0 24 } \
      BLE  { Group0 2f } \
      BLO  { Group0 25 } \
      BLS  { Group0 23 } \
      BLT  { Group0 2d } \
      BMI  { Group0 2b } \
      BNE  { Group0 26 } \
      BPL  { Group0 2a } \
      BRA  { Group0 20 } \
      BRN  { Group0 21 } \
      BSR  { Group0 8d } \
      BVC  { Group0 28 } \
      BVS  { Group0 29 } \
      ASL  { Group1 78 68 1868 } \
      ASR  { Group1 77 67 1867 } \
      CLR  { Group1 7f 6f 186f } \
      COM  { Group1 73 63 1863 } \
      DEC  { Group1 7a 6a 186a } \
      INC  { Group1 7c 6c 186c } \
      JMP  { Group1 7e 6e 186e } \
      LSL  { Group1 78 68 1868 } \
      LSR  { Group1 74 64 1864 } \
      NEG  { Group1 70 60 1860 } \
      ROL  { Group1 79 69 1869 } \
      ROR  { Group1 76 66 1866 } \
      TST  { Group1 7d 6d 186d } \
      STA  { Group2 97 b7 a7 18a7 } \
      STAA { Group2 97 b7 a7 18a7 } \
      STAB { Group2 d7 f7 e7 18e7 } \
      STD  { Group2 dd fd ed 18ed } \
      STS  { Group2 9f bf af 18af } \
      STX  { Group2 df ff ef cdef } \
      STY  { Group2 18df 18ff 1aef 18ef } \
      JSR  { JSR 9d bd ad 18ad } \
      ADCA { Group3 89 99 b9 a9 18a9 } \
      ADCB { Group3 c9 d9 f9 e9 18e9 } \
      ADDA { Group3 8b 9b bb ab 18ab } \
      ADDB { Group3 cb db fb eb 18eb } \
      ANDA { Group3 84 94 b4 a4 18a4 } \
      ANDB { Group3 c4 d4 f4 e4 18e4 } \
      BITA { Group3 85 95 b5 a5 18a5 } \
      BITB { Group3 c5 d5 f5 e5 18e5 } \
      CMPA { Group3 81 91 b1 a1 18a1 } \
      CMPB { Group3 c1 d1 f1 e1 18e1 } \
      EORA { Group3 88 98 b8 a8 18a8 } \
      EORB { Group3 c8 d8 f8 e8 18e8 } \
      LDA  { Group3 86 96 b6 a6 18a6 } \
      LDAA { Group3 86 96 b6 a6 18a6 } \
      LDAB { Group3 c6 d6 f6 e6 18e6 } \
      ORAA { Group3 8a 9a ba aa 18aa } \
      ORAB { Group3 ca da fa ea 18ea } \
      SBCA { Group3 82 92 b2 a2 18a2 } \
      SBCB { Group3 c2 d2 f2 e2 18e2 } \
      SUBA { Group3 80 90 b0 a0 18a0 } \
      SUBB { Group3 c0 d0 f0 e0 18e0 } \
      ADDD { Group4 c3 d3 f3 e3 18f3 } \
      CPD  { Group4 1a83 1a93 1ab3 1aa3 cda3 } \
      CMPD { Group4 1a83 1a93 1ab3 1aa3 cda3 } \
      CPX  { Group4 8c 9c bc ac cdac } \
      CPY  { Group4 188c 189c 18bc 1aac 18ac } \
      LDD  { Group4 cc dc fc ec 18ec } \
      LDS  { Group4 8e 9e be ae 18ae } \
      LDX  { Group4 ce de fe ee cdee } \
      LDY  { Group4 18ce 18de 18fe 1aee 18ee } \
      SUBD { Group4 83 93 b3 a3 18a3 } \
      BSET { Group5 14 1c 181c } \
      BCLR { Group5 15 1d 181d } \
      BRSET { Group6 12 1e 181e } \
      BRCLR { Group6 13 1f 181f } \
]

# ----- Utility functions

# Reduce all sequences of tab and space to single spaces
proc normalize { s } {
   set r ""
   set i 0

   # make only one kind of whitespace (damn it!!)
   set s [string map {"\t" " "} $s]

   # eat all sequences of spaces
   while { $i < [string length $s] } {
      set c [string index $s $i]
   
      append r $c
      incr i
   
      if { $c == " " } {
        while { [string index $s $i] == " " } { incr i }
      }
   }
   return $r
}

# Split string into groups of n
# splitevery 3 "ABCDEFGHIJKLMN"
# ABC DEF GHI JKL MN
proc splitevery { n String } {
   set origString $String
   set result {}
   for { set i 0 } { $i < [string length $origString] } { incr i $n } {
      lassign [splitat $n $String] chunk String
      lappend result $chunk
   }
   if { [string length $String] > 0 } {
      lappend result $String
   }
   return $result
}

# Split a string at index n into a two-element list
# splitat 3 "ABCDEFG"
# ABC DEFG
proc splitat { n String } {
   return [list [string range $String 0 $n-1] [string range $String $n end]]
}

# ----- Expression evaluation

# Return evaluated expression
proc ex { exp } {
   # character constants
   if { [string index $exp 0 ] == "'" } {
      scan [string index $exp 1] %c
   } else {
      expr [join [expression [tokenize $exp]]]
   }
}

# Split into a list along +, -, *, / and include these as elements
proc tokenize { s } {
   
   set accum ""
   set tokens {}
   
   for { set i 0 } { $i < [string length $s] } { incr i } {
      
      set c [string index $s $i]
      
      switch $c {
         "+" -
         "-" -
         "*" -
         "/" {
               if { [string length $accum] > 0 } {
                  lappend tokens $accum
                  set accum ""
               }
               lappend tokens $c
            }
         default { append accum $c }
      }  
   }
   # last term
   if { [string length $accum] > 0 } {
      lappend tokens $accum
   }
   return $tokens
}

# expression = term { termop term }
proc expression { tokens } {
   set i 0
   set result {}
   set noftokens [llength $tokens]
   
   lappend result [term [lindex $tokens $i]]
   
   incr i
   
   while { $i < $noftokens } {
      set token [lindex $tokens $i]
      switch $token {
         "+" -
         "-" -
         "*" -
         "/" { lappend result $token
               incr i
         }
         default {
            Error "Not a recognized math operator"
         }
      }
      if { $i == $noftokens } {
         Error "Dangling math operator"
      } else {
         lappend result [term [lindex $tokens $i]]
         incr i
      }
   }
   return $result
}

# term = symbol | number | *
proc term { trm } {
   global pc
   global symbols
   
   if { $trm == "*" } {
      set result $pc
   } elseif { [lsearch -exact [array names symbols] $trm] != -1 } {
      set result $symbols($trm)
   } elseif { ![string is integer [set result [number $trm]]] } {
      Error "Unable to evaluate $trm"
   }
   
   return $result
}

# Evaluate binary, octal, decimal, hex numbers
proc number { argument } {
   
    # Do substitutions
   switch [string index $argument 0] {
      "$"  { set argument [string replace $argument 0 0 "0x"] }
      "%"  { set argument [string replace $argument 0 0 "0b"] }
      "@"  { set argument [string replace $argument 0 0 "0o"] }
   }
   # Return original argument if cannot convert
   if {[catch { set result [expr $argument] }]} {
      set result $argument
   }
   return $result
}

# ----- Assembler helper functions

# Define / redefine a symbol
proc makesymbol { key value } {
   global symbols
      
   if { [string length $key] > 0 } {
      set label [string trimright $key ":"]
      set symbols($label) $value
   }
}

# Add assembled bytes to listing & to S19 file
proc output { s } {
   global pc
   
   set bytes [splitevery 2 $s]

   listing "[format %04x $pc] $bytes"
  
   S1 append $bytes
}

# Append an assembled line to listing
proc listing { s } {
   global Listing
   append Listing $s
}

# Move the program counter forward
proc incrpc { nofbytes } {
   global pc
   incr pc $nofbytes
}

# Bail out with error message
proc Error { userstring } {
   global lineno
   global line
   global pass
   
   puts "ERROR: $userstring"
   puts "on pass $pass when assembling"
   puts "\"$line\""
   puts "on line $lineno"
   exit
}

# ----- S19 tools

proc S1 { cmd args } {
   
   switch $cmd {
      "append" { S1append $args }
      "flush" { S1flush }
   }
}

proc S1append { bytes } {
   global pc
   global S19
   global S1record
      
   # Start a new record?
   if { [llength $S1record] == 0 } {
      lappend S1record [format %04X $pc]
   }
   set bytes [join $bytes]
   # Add bytes
   foreach byte $bytes {
      if { [llength $S1record] >= 33 } {
         # Emit current record
         set length [format %02X [expr [llength $S1record] + 2]]
         set bytestring "$length[join $S1record ""]"
         append S19 "S1$bytestring[chksum $bytestring]\r\n"
         # Set up next record
         set address "0x[lindex $S1record 0]"
         set S1record {}
         lappend S1record [format %04X [expr $address + 32]]
      }
      lappend S1record [string toupper $byte]
   }   
}

# Write out the last partial record
proc S1flush {} {
   global S19
   global S1record
   
   if { [llength $S1record] > 0 } {
      set length [format %02X [expr [llength $S1record] + 2]]
      set bytestring "$length[join $S1record ""]"
      set chksum [chksum $bytestring]
      append S19 "S1$bytestring$chksum\r\n"
      set S1record {}
   }
}

# S19 record checksum
proc chksum { bytestring } {
   
   set bytelist [splitevery 2 $bytestring]
   set total 0
   
   foreach byte $bytelist {
      scan $byte %2x byte
      incr total $byte
   }

   string range [format "%02X" [expr ~$total]] end-1 end
   
}

# ----- Directives: ORG, EQU, RMB, FCC, FCB, FDB

proc ORG { columns pass } {
   global pc
   
   lassign $columns label directive expression
   set value [ex $expression]
   
   if { $pass == 1 } {
      # overwrite pc as symbol value
      makesymbol $label $value
   } else {
      listing [format "%04x" $value]
      S1 flush
   }
   set pc $value
}

proc EQU { columns pass } {
   
   lassign $columns label directive expression
   set value [ex $expression]
   
   if { $pass == 1 } {
      # overwrite pc as symbol value
      makesymbol $label $value
   } else {
      listing [format "%04x" $value]
   }
}

proc RMB { size pass } {
   global pc
      
   if { $pass == 2 } {
      listing [format "%04x" $pc]
   }
   incrpc $size
}

proc FCC { line pass } {
   
   set idx [string first "FCC" $line]
   set arg [string trimleft [string range $line $idx+3 end]]
   if { [string length $arg] == 0 } {
      Error "No argument to FCC"
   }
   
   # Don't replace with string trim. It doesn't work.
   switch [string index $arg 0 ] {
      "'"  { set temp [string range $arg 1 end ]
             set argument [string range $temp 0 [string first "'" $temp]-1]
           } 
      "\"" { set temp [string range $arg 1 end ]
             set argument [string range $temp 0 [string first "\"" $temp]-1]
           }         
      "/" { set temp [string range $arg 1 end ]
            set argument [string range $temp 0 [string first "/" $temp]-1]
          }
      default { Error "No enclosing characters" }
   }
      
   if { $pass == 2 } {
      set output ""
      foreach char [split $argument ""] {
         append output [format "%02x" [scan $char %c]]
      }
      output $output
   }
   incrpc [string length $argument]
}

proc FCB { arg pass } {
   
   set byteList [split $arg ,]

   if { $pass == 2 } {
      foreach byte $byteList {
         set el [lindex [split [string trim $byte] " "] 0]
         append output [format "%02x" [ex $el]]
      }
      output $output
   }
   incrpc [llength $byteList]
}

proc FDB { arg pass } {
   
   set doubleByteList [split $arg ,]
   if { $pass == 2 } {
      foreach doubleByte $doubleByteList {
         set el [lindex [split [string trim $doubleByte] " "] 0]
         append output [format "%04x" [ex $el]]
      }
      output $output
   }
   incrpc [expr { [llength $doubleByteList] * 2 } ]
}

# ----- Addressing Modes

proc Inherent { opcode arg pass } {
   
   if { $pass == 2 } {
      output $opcode
   }
   incrpc [expr { [string length $opcode] / 2 } ]
}

proc Immediate8 { opcode arg pass } {
   
   # Ditch leading #
   set arg [string range $arg 1 end]
   if { $pass == 2 } {
      output "$opcode[format %02x [ex $arg]]"
   }
   incrpc [expr { [string length $opcode] / 2 + 1 } ]
}

proc Immediate16 { opcode arg pass } {
   
   # Ditch leading #
   set arg [string range $arg 1 end]

   if { $pass == 2 } {
      output "$opcode[format %04x [ex $arg]]"
   }
   incrpc [expr { [string length $opcode] / 2 + 2 } ]
}

proc Relative { opcode arg pass } {
   global pc
   
   if { $pass == 2 } {
      set offset [string range [format "%02x" [expr { [ex $arg] - ($pc+2) }]] end-1 end]
      output "$opcode$offset"
   }
   incrpc [expr { [string length $opcode] / 2 + 1 } ]
}

proc Direct { opcode arg pass } {
   
   if { $pass == 2 } {
      output "$opcode[format %02x [ex $arg]]"
   }
   incrpc [expr { [string length $opcode] / 2 + 1 } ]
}

proc Extended { opcode arg pass } {
      
   if { $pass == 2 } {
      output "$opcode[format %04x [ex $arg]]"
   }
   incrpc [expr { [string length $opcode] / 2 + 2 } ]
}

proc Indexed { indX indY arglist pass } {
   
   lassign $arglist offset index
   set index [string toupper $index]
   
   if { $index == "X" } {
      set opcode $indX
   } elseif { $index == "Y" } {
      set opcode $indY
   } else {
      Error "Unrecognized index register"
   }

   if { $pass == 2 } {
      if { [string length $offset] > 0 } {
         set out [ex $offset]
      } else {
         set out 0
      }
      output "$opcode[format %02x $out]"
   }
   incrpc [expr { [string length $opcode] / 2 + 1 } ]
}

# ----- Instruction Groups

proc Group0 { opcode arg pass } {
   
   if { ![string length $arg] > 0 } {
      Error "No argument"
   }
   Relative $opcode $arg $pass
}

proc Group1 { extended indX indY arg pass } {
   
   if { ![string length $arg] > 0 } {
      Error "No argument"
   }
   
   # Handle addressing mode
   set arglist [split $arg ,]
   if { [llength $arglist] > 1 } {
      Indexed $indX $indY $arglist $pass
   } else {
      Extended $extended $arg $pass
   }
}

proc Group2 { direct extended indX indY arg pass } {
   
   if { ![string length $arg] > 0 } {
      Error "No argument"
   }
   
   # Handle addressing mode
   set arglist [split $arg ,]
   if { [llength $arglist] > 1 } {
      Indexed $indX $indY $arglist $pass
   } else {
      DirectOrExtended $direct $extended $arg $pass
   }
}

proc Group3 { immediate direct extended indX indY arg pass } {
   
   if { ![string length $arg] > 0 } {
      Error "No argument"
   }
   
   # Handle addressing mode
   if { [string index $arg 0] == "#" } {
      Immediate8 $immediate $arg $pass
   } else {
      set arglist [split $arg ,]
      if { [llength $arglist] > 1 } {
         Indexed $indX $indY $arglist $pass
      } else {
         DirectOrExtended $direct $extended $arg $pass
      }
   }
}

proc Group4 { immediate direct extended indX indY arg pass } {
      
   if { ![string length $arg] > 0 } {
      Error "No argument"
   }
   
   # Handle addressing mode
   if { [string index $arg 0] == "#" } {
      Immediate16 $immediate $arg $pass
   } else {
      set arglist [split $arg ,]
      if { [llength $arglist] > 1 } {
         Indexed $indX $indY $arglist $pass
      } else {
         DirectOrExtended $direct $extended $arg $pass
      }
   }
}

# OPCODE direct mask
# OPCODE offset,X/Y mask
proc Group5 { direct indX indY arg pass } {
   
   # first arg is address
   set arg1 [lindex $arg 0]
   lassign [split $arg1 ,] offset index
   set index [string toupper $index]
   
   if { $index == "X" } {
      set opcode $indX
      if { [string length $offset] == 0 } {
         set address 0
      } else {
         set address $offset
      }
   } elseif { $index == "Y"} {
      set opcode $indY
      if { [string length $offset] == 0 } {
         set address 0
      } else {
         set address $offset
      }
   } elseif { $index == "" } {
      set opcode $direct
      set address $arg1
      if { ![string length $address] > 0 } {
         Error "Missing address argument"
      }         
   } else {
      Error "Unrecognized index register"
   }

   if { $pass == 2 } {
      # second arg is mask
      set mask [string trimleft [lindex $arg 1] "#"]
      if { ![string length $mask] > 0 } {
         Error "Missing mask argument"
      }
      output "$opcode[format %02x [ex $address]][format %02x [ex $mask]]"
   }
   incrpc [expr { [string length $opcode] / 2 + 2 } ]
}

# OPCODE direct mask address
# OPCODE offset,X/Y mask address
proc Group6 { direct indX indY arg pass } {
   global pc
   
   # first arg is address
   set arg1 [lindex $arg 0]
   lassign [split $arg1 ,] offset index 
   set index [string toupper $index]

   if { $index == "X" } {
      #set mode indexed
      set opcode $indX
      if { [string length $offset] == 0 } {
         set address 0
      } else {
         set address $offset
      }
   } elseif { $index == "Y"} {
      #set mode indexed
      set opcode $indY
      if { [string length $offset] == 0 } {
         set address 0
      } else {
         set address $offset
      }
   } elseif { $index == "" } {
      #set mode direct
      set opcode $direct
      set address $arg1
      if { ![string length $address] > 0 } {
         Error "Missing address argument"
      }         
   } else {
      Error "Unrecognized index register"
   }

   set nofbytes [expr { [string length $opcode] / 2 + 3 } ]
   
   if { $pass == 2 } {
      # second arg is mask
      set mask [string trimleft [lindex $arg 1] "#"]
      if { ![string length $mask] > 0 } {
         Error "Missing mask argument"
      }
      # third arg is branch address
      set branchaddr [lindex $arg 2]
      if { ![string length $branchaddr] > 0 } {
         Error "Missing branch argument"
      }
      set relativeaddr [string range [format "%02x" [expr { [ex $branchaddr] - ($pc+$nofbytes) }]] end-1 end]
      output "$opcode[format %02x [ex $address]][format %02x [ex $mask]]$relativeaddr"
   }
   
   incrpc $nofbytes
}

# ----- Misc
# JSR, DirectOrExtended

# Group 2 except allow DIRECT only if forced
proc JSR { direct extended indX indY arg pass } {   
   
   if { ![string length $arg] > 0 } {
      Error "JSR: No argument"
   }
   
   # Handle addressing mode
   set arglist [split $arg ,]
   if { [llength $arglist] > 1 } {
      Indexed $indX $indY $arglist $pass
   } else {
      if { [string index $arg 0 ] == "<" } {
         Direct $direct [string range $arg 1 end] $pass
      } else {
         if { [string index $arg 0] == ">" } {
            set arg [string range $arg 1 end]
         }
         Extended $extended $arg $pass
      }
   }
}

proc DirectOrExtended { direct extended arg pass } {

   # Force addressing mode
   if { [string index $arg 0] == ">" } {
      Extended $extended [string range $arg 1 end] $pass
   } elseif { [string index $arg 0 ] == "<" } {
      Direct $direct [string range $arg 1 end] $pass
   } else {
      # Try to detect addressing mode
      if { [ex $arg] < 256} {
         Direct $direct $arg $pass
      } else {
         Extended $extended $arg $pass
      }
   }
}

# ----- Assembler main functions

proc assemble { line pass } {
   global pc
   global opcodes
   
   # Ditch any ; comments
   set cidx [string first ";" $line]
   if { $cidx != -1 } {
      set line [string range $line 0 $cidx-1]
   }
   
   # Column 0: label
   # Column 1: Instruction (mnemonic or directive)
   # Column 2: First argument
   # Column 3: Second argument
   # Column 4: Third argument
   set columns [split [normalize $line] " "]
   
   if { $pass == 1 } {
      makesymbol [lindex $columns 0] $pc
   }

   set instruction [string toupper [lindex $columns 1]]
   
   if { $instruction != "" } {
      switch $instruction {
         "ORG" -
         "EQU" { $instruction $columns $pass }
         "RMB" { $instruction [ex [lindex $columns 2]] $pass }
         "FCB" -
         "FDB" { $instruction [join [lrange $columns 2 end] " "] $pass }
         "FCC" { $instruction $line $pass }
         "BSET" -
         "BCLR" -
         "BRSET" -
         "BRCLR" { eval $opcodes($instruction) {[lrange $columns 2 end]} $pass }
         default {
            if { [string index $instruction 0] != ";" } {
               eval $opcodes($instruction) {[lindex $columns 2]} $pass
            }
         }
      }
   }
}

# ----- Global variables

set pc 0
array set symbols {}
set S19 ""
set Listing ""
set S1record {}
set line ""

# ----- Main Program

# Command line argument parsing

if { $argc == 0 } {
   puts {Usage: assembler file1 file2 ... [ - option1 option2 ...]}
   exit 
}

set options {}
for { set argno 0 } { $argno < $argc } { incr argno } {
   set arg [lindex $argv $argno]
   if { [string index $arg 0] == "-"} {
      lappend options [string trimleft $arg "-"]
      incr argno
      while { $argno < $argc } {
         lappend options [lindex $argv $argno]
         incr argno
      }
      break
   }
   lappend files $arg
}

# Set options
set listing_option [lsearch -exact $options "l"]
set symbol_option [lsearch -exact $options "s"]

# Assemble each filename & append to a common listing & S19 file
foreach filename $files {
   puts $filename
   set fp [open $filename]

   set pass 1
   for { set lineno 1 } { -1 != [gets $fp line] } { incr lineno } {
      if { [string length $line] > 0 } {
         if { [string index $line 0] != "*" } {
            assemble $line $pass
         }
      }
   }

   set pass 2
   seek $fp 0
   for { set lineno 1 } { -1 != [gets $fp line] } { incr lineno } {
      listing "[format "%04d" $lineno] "
      if { [string length $line] > 0 } {
         if { [string index $line 0] != "*" } {
            # puts "$lineno $line"
            assemble $line $pass
         } else {
            listing $line
         }
      }
      listing "\n"
   }
   close $fp
   S1 flush
}

# Output the S9 record
append S19 "S9030000FC\r\n"

# Write S19 file
set S19filename "[lindex [split [lindex $files 0] .] 0].S19"
set fd [open $S19filename "w"]
puts -nonewline $fd $S19
close $fd

# Print listing
if { $listing_option != -1 } {
   puts $Listing
}

# Print symbol table
if { $symbol_option != -1 } {
   foreach symbol [lsort [array names symbols]] {
      puts "[format %-11s $symbol] [format %04x $symbols($symbol)]"
   }
}