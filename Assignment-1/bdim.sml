exception Division; (*Exception for division by zero*)
exception IndexOutOfBound; (*Exception for negative indices and indices > length of data structure*)

fun interpret(filename : string) =  (*This is the function that does everything*)
    let
      fun helper(file : string) =  (*This function will help in taking inputs*)
        let
          val instream = TextIO.openIn(filename); (*Opens an input stream in the given filename*)
          val line = TextIO.inputLine(instream); 
          val l = [] ; (*Empty list that will contain the lines of file, but in reverse order*)
          fun read(ls, lin, stream) = 
                let
                    val lin1 = TextIO.inputLine(stream);
                    val ls1 = valOf(lin)::ls
                in
                    if(lin1 = NONE) then ls1
                    else read(ls1, lin1, stream)
                end;
        in
          read(l, line, instream)
        end;
        val mem = Array.array(64,0); (*Memory array*)
        val temp = helper(filename); (*List that contains the lines of the file*)
        val code_list = rev(temp); (*List that is reversed of the previous list*)
        val code = Vector.fromList(code_list); (*Converting the list to a vector to access elements in O(1) time *)
        val len = Vector.length(code); (*No of lines in the input file*)
        fun intToBool(i) =  (*Function to covert integer to bool*)
            if(i = 1) then true else false;
        fun boolToInt(i) = (*Function to convert bool to integer*)
            if(i = false) then 0 else 1;

        fun execute((opcode, opd1, opd2, tgt)) = (*This is the function where each quadraple is processed*)
              if(opcode = 1) then  (*Take input from the user interactively*)
              let 
              val print_message = print("Please enter the input: ");
              val inp = valOf(TextIO.inputLine TextIO.stdIn);
              val im = valOf (Int.fromString(inp))
              in Array.update(mem, tgt, im) end
              else if (opcode = 2) then (*mem[tgt] :- mem[opd1]*)
              if(opd1 < 0 orelse tgt < 0 orelse opd1 > Array.length(mem) orelse tgt > Array.length(mem)) then raise IndexOutOfBound
              else Array.update(mem, tgt, Array.sub(mem,opd1)) 
              else if (opcode = 3) then (*mem[tgt] :- not mem[opd1]*)
              if(opd1 < 0 orelse tgt < 0 orelse opd1 > Array.length(mem) orelse tgt > Array.length(mem)) then raise IndexOutOfBound
              else let val op3 = boolToInt(Bool.not(intToBool(Array.sub(mem,opd1))))
              in Array.update(mem, tgt, op3) end
              else if (opcode = 4) then (*mem[tgt] := mem[opd1] or mem[opd2]*)
              if(opd1 < 0 orelse opd2 < 0 orelse tgt < 0 orelse opd1 > Array.length(mem) orelse opd2 > Array.length(mem) orelse tgt > Array.length(mem)) then raise IndexOutOfBound
              else let val op4 = boolToInt(intToBool(Array.sub(mem,opd1)) orelse intToBool(Array.sub(mem,opd2)))
              in Array.update(mem, tgt, op4) end
              else if (opcode = 5) then (*mem[tgt] := mem[opd1] and mem[opd2]*)
              if(opd1 < 0 orelse opd2 < 0 orelse tgt < 0 orelse opd1 > Array.length(mem) orelse opd2 > Array.length(mem) orelse tgt > Array.length(mem)) then raise IndexOutOfBound
              else let val op5 = boolToInt(intToBool(Array.sub(mem,opd1)) andalso intToBool(Array.sub(mem,opd2)))
              in Array.update(mem, tgt, op5) end
              else if (opcode = 6) then (*mem[tgt] := mem[opd1] + mem[opd2]*)
              if(opd1 < 0 orelse opd2 < 0 orelse tgt < 0 orelse opd1 > Array.length(mem) orelse opd2 > Array.length(mem) orelse tgt > Array.length(mem)) then raise IndexOutOfBound
              else let val op6 = Array.sub(mem,opd1) + Array.sub(mem,opd2)
              in Array.update(mem, tgt, op6) end
              else if (opcode = 7) then (*mem[tgt] := mem[i] - mem[opd2]*)
              if(opd1 < 0 orelse opd2 < 0 orelse tgt < 0 orelse opd1 > Array.length(mem) orelse opd2 > Array.length(mem) orelse tgt > Array.length(mem)) then raise IndexOutOfBound
              else let val op7 = Array.sub(mem,opd1) - Array.sub(mem,opd2)
              in Array.update(mem, tgt, op7) end
              else if (opcode = 8) then (*mem[tgt] := mem[opd1] * mem[opd2]*)
              if(opd1 < 0 orelse opd2 < 0 orelse tgt < 0 orelse opd1 > Array.length(mem) orelse opd2 > Array.length(mem) orelse tgt > Array.length(mem)) then raise IndexOutOfBound
              else let val op8 = Array.sub(mem,opd1) * Array.sub(mem,opd2)
              in Array.update(mem, tgt, op8) end
              else if (opcode = 9) then (*mem[tgt] := mem[opd1] div mem[opd2]*)
              if(opd1 < 0 orelse opd2 < 0 orelse tgt < 0 orelse opd1 > Array.length(mem) orelse opd2 > Array.length(mem) orelse tgt > Array.length(mem)) then raise IndexOutOfBound
              else if(Array.sub(mem,opd2) = 0) then raise Division else
              let val op9 = Array.sub(mem,opd1) div Array.sub(mem,opd2)
              in Array.update(mem, tgt, op9) end
              else if (opcode = 10) then (*mem[tgt] := mem[opd1] mod mem[opd2]*)
              if(opd1 < 0 orelse opd2 < 0 orelse tgt < 0 orelse opd1 > Array.length(mem) orelse opd2 > Array.length(mem) orelse tgt > Array.length(mem)) then raise IndexOutOfBound
              else if(Array.sub(mem,opd2) = 0) then raise Division else
              let val op10 = Array.sub(mem,opd1) mod Array.sub(mem,opd2)
              in Array.update(mem, tgt, op10) end
              else if (opcode = 11) then (*mem[tgt] := (mem[opd1] = mem[opd2])*)
              if(opd1 < 0 orelse opd2 < 0 orelse tgt < 0 orelse opd1 > Array.length(mem) orelse opd2 > Array.length(mem) orelse tgt > Array.length(mem)) then raise IndexOutOfBound
              else let val op11 = boolToInt((Array.sub(mem,opd1)) = (Array.sub(mem,opd2)))
              in Array.update(mem, tgt, op11) end
              else if (opcode = 12) then (*mem[tgt] := (mem[opd1] > mem[opd2])*)
              if(opd1 < 0 orelse opd2 < 0 orelse tgt < 0 orelse opd1 > Array.length(mem) orelse opd2 > Array.length(mem) orelse tgt > Array.length(mem)) then raise IndexOutOfBound
              else let val op12 = boolToInt((Array.sub(mem,opd1)) > (Array.sub(mem,opd2)))
              in Array.update(mem, tgt, op12) end
              else if (opcode = 15) then (*print mem[opd1]*)
              if(opd1<0) then raise IndexOutOfBound
              else print(Int.toString(Array.sub(mem,opd1)))
              else if (opcode = 16) then (*mem[tgt] := v*)
              if(tgt<0) then raise IndexOutOfBound
              else Array.update(mem,tgt,opd1)
              else if(opcode = 0) then  print("\n") else print("")

        fun help(i, n, exec)= 
              if(i = n) then print("\n")
              else 
                  let
                    fun  idx(str, i) = 
                          if(String.sub(str,i) = #",") then i else idx(str, i+1);
                    fun value(str, i, j, out) = 
                          if(i=j) then out else value(str, i+1, j, out^String.str(String.sub(str,i)));
                     val veci = Vector.sub(code, i);
                     val idx1 = idx(veci, 1);
                     val idx2 = idx(veci, idx1+1);
                     val idx3 = idx(veci, idx2+1);
                     val val1 = value(veci, 1, idx1, "");
                     val val2 = value(veci, idx1+1, idx2, "");
                     val val3 = value(veci, idx2 + 1, idx3, "");
                     val val4 = value(veci, idx3+1, String.size(veci) - 1, "");
                     val opcode = valOf (Int.fromString(val1));
                     val opd1 = valOf (Int.fromString(val2));
                     val opd2 = valOf (Int.fromString(val3));
                     val tgt = valOf (Int.fromString(val4));
                     val ans = exec((opcode,opd1,opd2,tgt))
                  in
                    if(opcode = 13) then if(Array.sub(mem,opd1) = 1) then help(tgt, n, exec) else help(i+1,n,exec)
                    else if (opcode = 14) then help(tgt, n, exec)
                    else if (opcode = 0) then print("\n")
                    else
                    help(i+1,n,exec) handle Division => print(" ------------   Division by zero is not allowed!   ----------------\n") | IndexOutOfBound => print("-------------   Sorry, Index Out Of Bound! -------------- \n") | Overflow => print("Oops! Too large input. Please enter a small input! \n")
                  end    
    in
      help(0,len,execute) 
    end;


fun main() = 
let
      val x =  print("\n ----------------  Please call the interpret function with file name as argument. Eg : interpret(\"ap.bdim\") :  " );
      val file = valOf(TextIO.inputLine TextIO.stdIn);
      val filename = String.substring(file, 11, String.size(file) - 14)
  in
      interpret(filename)
  end;

main()