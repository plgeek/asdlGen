signature ZTV =
sig
    val main : string * string list -> OS.Process.status
end

structure Ztv : ZTV =
struct
    structure Z  = zsuif
    structure W  = Word
    structure L  = List
    structure I  = Int
    structure ID = Identifier

    exception BadVariableEntry
    exception BadProcedureEntry
    exception MissingSymbolDefinition
    exception MissingTypeDefinition
    exception BadRepeatValueBlock
    exception BadCharacterInConstantBitString
    exception UnknownSizeInEmitConstants
    exception BadBitAlignment
    exception MissingProcedureBody
    exception BadParameterInFunctionParamList
    exception Can'tDoItYet
    exception NoRefName of string
    exception BadRegister
    exception BadLocation
    exception ShouldNotBeUsed

    val sort = ListMergeSort.sort

    fun curry f x y = f (x, y)
    fun uncurry f (x, y) = f x y
    fun swap f (x, y) = f (y, x)

    val toUpper = String.map Char.toUpper

    datatype RegType = Int8Bit
                     | Int16Bit
                     | Int32Bit
                     | Fp32Bit
                     | Fp64Bit

    datatype Label = AssemblyLab of string
                   | ControlLab of string

    datatype SourceTypes = Reg of RegType * int
                         | Loc of int * bool
                         | Glo of int
                         | Lab of Label

    fun regTytoString Int8Bit  = "0"
      | regTytoString Int16Bit = "1"
      | regTytoString Int32Bit = "2"
      | regTytoString Fp32Bit  = "3"
      | regTytoString Fp64Bit  = "4"

    fun regTytoName Int8Bit  = "Int8Bit"
      | regTytoName Int16Bit = "Int16Bit"
      | regTytoName Int32Bit = "Int32Bit"
      | regTytoName Fp32Bit  = "Fp32Bit"
      | regTytoName Fp64Bit  = "Fp64Bit"

    fun regTytoSize Int8Bit  = 1
      | regTytoSize Int16Bit = 2
      | regTytoSize Int32Bit = 4
      | regTytoSize Fp32Bit  = 4
      | regTytoSize Fp64Bit  = 8

    val globalLevel = "0"
    val paramLevel  = "1"
    val localLevel  = "2"

    val argNum  = ref 0
    val procNum = ref 1

    fun getTypeKey {key = key, value = _} = key

    fun getSymbolKey symte =
        #uid (case symte of
                  Z.CodeLabelEntry {key = key, ...} => key
                | Z.ProcedureEntry {key = key, ...} => key
                | Z.RegisterEntry  {key = key, ...} => key
                | Z.VariableEntry  {key = key, ...} => key
                | Z.ParameterEntry {key = key, ...} => key
                | Z.FieldEntry     {key = key, ...} => key
                | Z.NestedVariable {key = key, ...} => key)

    structure H = HashTableFn (type hash_key = StdTypes.nat
                               val hashVal = W.fromInt
                               val sameKey = op=)

    val outFile = ref TextIO.stdOut

    fun withOpenFile f fname = let
        val out = BinIO.openIn fname
    in
        (f out before BinIO.closeIn out)
        handle e => (BinIO.closeIn out;
                     TextIO.closeOut (!outFile);
                     raise e)
    end

    fun complain s = (TextIO.output (TextIO.stdErr, s);
                      TextIO.output (TextIO.stdErr, "\n"))

    local
        val primeNum = 123
    in
        val typeTab : Z.type_table_entry H.hash_table ref =
            ref (H.mkTable(primeNum, MissingTypeDefinition))
        val symbolTab : {symbol : Z.symbol_table_entry,
                         refname : SourceTypes option ref} H.hash_table ref =
            ref (H.mkTable(primeNum, MissingSymbolDefinition))
        val externSymbolTab :
            {symbol : Z.symbol_table_entry,
             refname : SourceTypes option ref} H.hash_table ref =
            ref (H.mkTable(primeNum, MissingSymbolDefinition))

        fun initTabs () =
            (typeTab         := H.mkTable(primeNum, MissingTypeDefinition);
             symbolTab       := H.mkTable(primeNum, MissingSymbolDefinition);
             externSymbolTab := H.mkTable(primeNum, MissingSymbolDefinition))
    end

    local
        fun prTkey typ = (getTypeKey typ, typ)
        fun prSkey sym = (getSymbolKey sym, {symbol = sym,
                                             refname = ref NONE})
        fun prEkey sym = (getSymbolKey sym, {symbol = sym,
                                             refname = ref NONE})
    in
        fun buildTypeTab entries =
            app (fn elem => H.insert (!typeTab) (prTkey elem)) entries

        fun buildSymbolTab entries =
            app (fn elem => H.insert (!symbolTab) (prSkey elem)) entries

        fun buildExternSymbolTab entries =
            app (fn elem => H.insert (!externSymbolTab) (prEkey elem)) entries
    end

    fun bitsToBytes d =
        let
            fun char2hex c =
                if c < #"0" orelse c > #"f" then
                    raise BadCharacterInConstantBitString
                else ord c - ord #"0"

            fun getByte(hi, lo) =  16 * (ord hi) + ord lo

            fun pair [] = []
              | pair (a :: b :: rest) = (a, b) :: pair rest
              | pair _ = raise (Fail "bad string length")
                
        in
            map getByte (pair (explode d))
        end

    fun findType key = H.lookup (!typeTab) key
    fun findSymbol {uid = key, name = name} =
        H.lookup (!symbolTab) key handle _ => H.lookup (!externSymbolTab) key

    fun getRegType (Z.Data data_type) =
        let
            fun intRegTyp (Z.Int 8)  = (Int8Bit, 1)
              | intRegTyp (Z.Int 16) = (Int16Bit, 2)
              | intRegTyp (Z.Int 32) = (Int32Bit, 4)
              | intRegTyp (Z.Int n)  =
                raise (Fail
                       ("Unknown bit size in intRegTyp " ^ (I.toString n)))
              | intRegTyp _  = raise (Fail "Not an Int type in getRegTyp")

            fun fpRegTyp (Z.Int 32) = (Fp32Bit, 4)
              | fpRegTyp (Z.Int 64) = (Fp64Bit, 8)
              | fpRegTyp _  = raise (Fail "Unknown bit size in fpRegType")
        in
            case data_type of
                Z.Boolean_type {bit_size = bitSize,
                                bit_alignment = bitAlignment} =>
                ((Int32Bit, #2 (intRegTyp bitSize)) handle e => raise e)
                
              | Z.Integer_type {bit_size = bitSize,
                                bit_alignment = bitAlignment} =>
                (intRegTyp bitSize handle e => raise e)

              | Z.UInteger_type {bit_size = bitSize,
                                 bit_alignment = bitAlignment} =>
                ((intRegTyp bitSize) handle e => raise e)

              | Z.Floating_point_type {bit_size = bitSize,
                                       bit_alignment = bitAlignment} =>
                ((fpRegTyp bitSize)  handle e => raise e)

              | Z.Enumerated_type {bit_size = bitSize,
                                   bit_alignment = bitAlignment,
                                   name = name,
                                   cases = cases} =>
                ((Int32Bit, #2 (intRegTyp bitSize)) handle e => raise e)

              | Z.Pointer_type {bit_size = bitSize,
                                bit_alignment = bitAlignment,
                                reference_type = referenceType} =>
                ((intRegTyp bitSize) handle e => raise e)

              | Z.Array_type {bit_size = bitSize,
                              bit_alignment = bitAlignment,
                              element_type = elementType,
                              lower_bound = lowerBound,
                              upper_bound = upperBound} =>
                ((Int32Bit, 4) handle e => raise e)

              | Z.Group_type {bit_size = bitSize,
                              bit_alignment = bitAlignment,
                              name = name,
                              fields = fields} =>
                ((Int32Bit, 4) handle e => raise e)
        end

  | getRegType (Z.Procedure proc_type) = (Int32Bit, 4)

  | getRegType (Z.Qualified {qualifications = qualifications,
                             type' = typ}) = raise Can'tDoItYet
    
  | getRegType (Z.Void) = raise Can'tDoItYet

    fun typIdtoRegty type_id =
        let
            val {key = _, value = typ} = findType type_id
        in
            #1 (getRegType typ)
        end

    fun regTypeSte (Z.CodeLabelEntry _) = Int32Bit
      | regTypeSte (Z.ProcedureEntry _) = Int32Bit
      | regTypeSte (Z.RegisterEntry _)  = Int32Bit
      | regTypeSte (Z.VariableEntry x)  = #1(getRegType (#type' (#def x)))
      | regTypeSte (Z.ParameterEntry x) = typIdtoRegty (#type' x)
      | regTypeSte _ = raise Can'tDoItYet

    fun getAlignment (Z.Data dataType) =
        let val v =
            case dataType of
                Z.Boolean_type x        => #bit_alignment x
              | Z.Integer_type x        => #bit_alignment x
              | Z.UInteger_type x       => #bit_alignment x
              | Z.Floating_point_type x => #bit_alignment x
              | Z.Enumerated_type x     => #bit_alignment x
              | Z.Pointer_type x        => #bit_alignment x
              | Z.Array_type x          => #bit_alignment x
              | Z.Group_type x          => #bit_alignment x
        in
            case v of
                Z.Int(n) => n
              | _ => raise BadBitAlignment
        end
      | getAlignment (Z.Procedure procedureType) = 32
      | getAlignment (Z.Qualified qualifications) = 32
      | getAlignment (Z.Void) =
        raise (Fail "Bad allignment in getAlignment")

    fun emit s     = TextIO.output (!outFile, s)
    fun newline () = emit "\n"
    fun tab     () = emit "\t"
    fun newstat () = emit "+"

    fun dataSection () = emit "-\t.seg\t\"data\"\n"
    fun textSection () = emit "-\t.seg\t\"text\"\n"
    fun align n = (emit "-\t.align\t"; emit (I.toString n); newline ())

    local
        val n = ref 1
    in
        fun initLabCount () = n := 1

        exception ProblemInLabel

        fun newLabel (NONE, _) =
            ControlLab("L" ^ I.toString(!n) before n := !n + 1)
          | newLabel (SOME {uid = _, name = label}, true) =
            AssemblyLab(ID.toString(label) ^ ":")
          | newLabel (SOME sym, false) =
            let 
                val {symbol = ste, refname = refname} = findSymbol sym
            in
                case !refname of
                    NONE =>
                        let
                            val labstr = "L" ^ I.toString(!n)
                                before n := !n + 1
                            val label  = ControlLab labstr
                        in
                            refname := SOME (Lab label);
                            label
                        end
                  | (SOME (Lab label)) => label
                  | _ => raise ProblemInLabel
            end

        fun getLabName (AssemblyLab name) = name
          | getLabName (ControlLab name)  = name

        fun emitLabel (AssemblyLab label) = (emit "-"; emit label; newline ())
          | emitLabel (ControlLab label) =  (emit label; newline ())
    end

    local
        val r = ref 32
    in
        datatype RegCase      = RU | RL
        fun initRegCount ()   = r := 32

        fun regToLetter (Reg (typ, _)) =
            (case typ of
                 Int8Bit  => "b"
               | Int16Bit => "w"
               | Int32Bit => "r"
               | Fp32Bit  => "f"
               | Fp64Bit  => "d")
          | regToLetter _ = raise (Fail "Non-Register passed to regToLetter")

        fun newFixedReg (typ, n) = Reg (typ, n)
        fun newReg typ           = Reg (typ, !r) before r := !r + 1
        fun regToString (r' as (Reg (typ, n)), rcase) =
            let
                val is = I.toString n
                val casfn = case rcase of
                                 RU => Char.toUpper
                               | RL => Char.toLower
                val casfn = String.map casfn
                val r = regToLetter r'
            in
                casfn r ^ "[" ^ is ^ "]"
            end
          | regToString _       = raise BadRegister

        fun emitReg' (reg, cas) = emit (regToString (reg, cas))
        fun emitReg reg         = emitReg' (reg, RL)
        fun useReg r = (emit "u"; emitReg r; newline ())
        val fp                  = newFixedReg (Int32Bit, 30)
        val sp                  = newFixedReg (Int32Bit, 14)
        val r0                  = newFixedReg (Int32Bit, 0)

        val f0                  = newFixedReg (Fp32Bit, 0)
        val d0                  = newFixedReg (Fp64Bit, 0)
        val r8                  = newFixedReg (Int32Bit, 8)
        val w8                  = newFixedReg (Int16Bit, 8)
        val b8                  = newFixedReg (Int8Bit, 8)

        datatype SparcPain      = Mul | Div | Rem
        val multi : SourceTypes option ref = ref NONE
        val divid : SourceTypes option ref = ref NONE
        val remen : SourceTypes option ref = ref NONE
        fun initMulDivOps () = (multi:=NONE; divid:=NONE; remen:=NONE)
    end

    fun regAssign (reg1, reg2, restore) =
        (newstat (); emitReg reg1; emit "=";
	 if restore then emit "RS[" else ();
         emitReg reg2;
         if restore then emit "]" else ();
         newline ())


    fun locToString (Loc (n, _)) = "LOC[" ^ (I.toString n) ^ "]"
      | locToString _            = raise BadLocation

    fun emitLoc loc = emit (locToString loc)

    fun gloToString (Glo n) = "GLO[" ^ (I.toString n) ^ "]"
      | gloToString _       = raise BadLocation

    fun emitGlo glo = emit (gloToString glo)

    fun emitWord n = (emit "-\t.word\t";
                      if n < 0 then emit "-" else ();
                          emit (I.toString (I.abs(n))); newline ())
    fun emitByte n = (emit "-\t.byte\t"; emit (I.toString n); newline ())

    local
        fun h (str, lab, typ) = (emit "-."; emit (getLabName lab); emit ":\t.";
                                 emit typ; emit "\t0r";
                                 emit str; newline ())
    in
        fun emitDouble (str, lab) = h (str, lab, "double")

        fun emitSingle (str, lab) = h (str, lab, "single")
    end

    fun emitProcDecl name = (emit "f"; emit name; newline ())

    fun emitPlusInf     () = emit "-\t.word\t2147483647\n"
    fun emitNegInf      () = emit "-\t.word\t-2147483648\n"
    fun emitUnsignedInf () = emit "-\t.word\t4294967295U\n"

    fun emitConstants (clist, size) =
(*        (print ("My size is " ^ (I.toString size)); *)
        case size of
            1 => app emitByte clist
          | 4 => app emitWord clist
          | _ => raise UnknownSizeInEmitConstants

    fun emitgDecl (name) = (emit "-\t.global\t"; emit name; newline())

    local
        val C = "Cd[0]d[2]d[4]d[6]d[8]d[10]d[12]d[14]d[16]d[18]\
                \d[20]d[22]d[24]d[26]d[28]d[30]\n"
        val U = "ur[1]\nur[2]\nur[3]\nur[4]\nur[5]\nur[6]\nur[7]\nur[8]\n\
                \ur[9]\nur[10]\nur[11]\nur[12]\nur[13]\nur[14]\nur[15]\
                \\nud[0]\n"
        val S = "Sr[1]r[2]r[3]r[4]r[5]r[6]r[7]r[8]r[9]r[10]r[11]r[12]\
                \r[13]r[14]r[15]d[0]d[2]d[4]d[6]d[8]d[10]d[12]d[14]\
                \d[16]d[18]d[20]d[22]d[24]d[26]d[28]d[30]\nt*\n"
    in
        fun doArgs regs =
            let
                val cDepth = ref 68
                val argCnt = ref 0
                fun incArgCnt () = argCnt := !argCnt + 1

                fun emtF (r, offset) =
                    (newstat ();
                     emit "F["; emitReg sp; emit "+"; emit (I.toString offset);
                     emit "]=HP["; emitReg r; emit "]"; newline ();
                     newstat();
                     emit "F["; emitReg sp; emit "+";
                     emit (I.toString (offset + 4));
                     emit "]=LP["; emitReg r; emit "]"; newline ())

                fun emtD (r, offset) =
                     (newstat (); emit "D["; emitReg sp; emit "+";
                      emit (I.toString offset); emit "]="; emitReg r;
                      newline ())

                fun emtFfl (r, offset) =
                    (newstat ();
                     emit "F["; emitReg sp; emit "+"; emit (I.toString offset);
                     emit "]="; emitReg r; newline())

                fun emitr (r, offset) =
                    (newstat ();
                     emitReg r; emit "=R["; emitReg sp; emit "+";
                     emit (I.toString offset); emit "]"; newline ())

                fun emitlr (r, offset) =
                    (newstat ();
                     emit "R["; emitReg sp; emit "+"; emit (I.toString offset);
                     emit "]="; emitReg r; newline ())

                fun doArg reg =
                    let
                        val offset = !cDepth
                        val (stInc, reglist) =
                            case reg of
                                (Reg (Fp64Bit, _))  =>
                                    let
                                        val emitfn = if (offset mod 8 <> 0)
                                                         then emtF 
                                                     else emtD
                                    in
                                        emitfn (reg, offset);
                                        if offset < 88 then
                                            let
                                                val r1 = newFixedReg
                                                    (Int32Bit, 8 + (!argCnt))
                                                val _ = incArgCnt ()
                                                val r2 = newFixedReg
                                                    (Int32Bit, 8 + (!argCnt))
                                                val _ = incArgCnt ()
                                            in
                                                emitr (r1, offset);
                                                useReg r1;
                                                emitr (r2, offset + 4);
                                                useReg r2;
                                                (8, [r1, r2])
                                            end
                                        else if offset = 88 then
                                            let
                                                val r = newFixedReg
                                                    (Int32Bit, 8 + (!argCnt))
                                                val _ = incArgCnt ()
                                            in
                                                emitr (r, offset);
                                                useReg r;
                                                (8, [r])
                                            end
                                        else
                                            (8, [])
                                    end

                              | (Reg (Fp32Bit, _))  =>
                                    (emtFfl (reg, offset);
                                     if offset < 92 then
                                         let
                                             val r = newFixedReg
                                                 (Int32Bit, 8 + (!argCnt))
                                             val _ = incArgCnt ()
                                         in
                                             emitr (r, offset);
                                             useReg r;
                                             (4, [r])
                                         end
                                     else
                                         (4, []))

                              | (Reg (_, _)) =>
                                    if offset < 92 then
                                        let
                                            val r = newFixedReg
                                                (Int32Bit, 8 + (!argCnt))
                                            val _ = incArgCnt ()
                                        in
                                            regAssign (r, reg, false);
                                            useReg r;
                                            (4, [r])
                                        end
                                    else (emitlr (reg, offset);
                                          (4, []))
                              | _ => raise (Fail "Bad Type in doArg")
                    in
                        cDepth := !cDepth + stInc;
                        reglist
                    end
                val reglist = foldr op@ [] (map doArg regs)
            in
                (reglist, !cDepth)
            end

        fun emitC () = emit C
        fun emitU () = emit U
        fun emitS () = emit S
    end

    fun emitVarPrelude ((sym as {uid = _, name = nm}) : Z.symbol, alignment) =
        let
            val newlabel = newLabel (SOME sym, true)
            val name     = ID.toString nm
        in
            dataSection ();
            align alignment;
            emitgDecl name;
            emitLabel newlabel
        end

    fun compileConst (Z.ConstInt sint, siz) =
        let
            val siz' = case siz of
                NONE => (case sint of
                             Z.Finite (_)   => 4
                           | Z.PlusInf      => 0
                           | Z.NegInf       => 0
                           | Z.UnsignedInf  => 0
                           | Z.Undetermined => 4)
              | SOME n => n
        in
            case sint of
                Z.Finite (n)   => emitConstants ([n], siz')
              | Z.PlusInf      => emitPlusInf ()
              | Z.NegInf       => emitNegInf ()
              | Z.UnsignedInf  => emitUnsignedInf ()
              | Z.Undetermined => emitConstants ([0], siz')
        end

      | compileConst  (Z.ConstString str, _) =
        emitConstants (map ord (explode str), 1)

      | compileConst (Z.ConstBits str, _) =
        emitConstants (bitsToBytes str, 1)

    fun getElemArrayType (Z.Data (Z.Array_type x)) =
        SOME (#element_type x)
      | getElemArrayType _ = NONE

    fun compileValueBlock (Z.Constant_value_block
                           {data_type = _, constant = constant},
                           siz) =
        compileConst (constant, siz)

      | compileValueBlock (Z.Expression_value_block
                           {data_type = _, expression = expression}, _) =
        (emit "-\t.word\t";
         case expression of
             Z.SrcDst {instr = Z.Load_address_instruction x, ...} =>
                 emit (ID.toString(#name(#addressed_symbol x)))
           | Z.SrcDst
  {instr = Z.Binary_arithmetic_instruction
   {binop = binop,
    source1 = Z.SrcDst {instr = Z.Load_address_instruction x, ...},
    source2 = Z.SrcDst {instr = Z.Load_constant_instruction
                                 {constant = Z.ConstInt (Z.Finite n), ...},
                                 ...},
    result_type = resultTyp,
    destination_op = destOp}, ...} =>
  (emit (ID.toString(#name(#addressed_symbol x)));
   emit "+"; emit (I.toString n))
               
          | _ => raise (Fail "Bad initialization in value block");

           newline ())

      | compileValueBlock (Z.Multi_value_block {data_type = dtype,
                                                inits = inits}, siz) =
        let
            fun cmp ({bit_offset = bo1, block = _},
                     {bit_offset = bo2, block = _}) = bo1 > bo2
            fun doBlock {bit_offset = bitOffset, block = valueBlock} =
                compileValueBlock (valueBlock, siz)
        in
            app doBlock (sort cmp inits)
        end

      | compileValueBlock (Z.Repeat_value_block {data_type = _,
                                                 count = Z.Finite n,
                                                 block = block}, siz) =
        let
            fun iter 0 = ()
              | iter n = (compileValueBlock (block, siz); iter  (n - 1))
        in
            iter n
        end

      | compileValueBlock (Z.Repeat_value_block {data_type = _, count = _,
                                                 block = block}, _) =
        raise BadRepeatValueBlock

      | compileValueBlock (Z.Undefined_value_block {data_type = typ}, siz) =
        compileConst (Z.ConstInt (Z.Finite 0), siz)

    fun compileVariable (sym, typ, valueBlock) =
        let
            val alignment = getAlignment typ
            val typOpt = getElemArrayType typ
            val siz = case typOpt of
                          SOME t => SOME(#2(getRegType t))
                        | NONE   => NONE
        in
            case (typ, valueBlock) of
                (Z.Data (Z.Array_type x), SOME (Z.Undefined_value_block _)) =>
                    let
                        val siz = case #bit_size x of
                                       Z.Int s => s
                                     | _ => raise (Fail "We failed")
                    in
                        emit "-\t.common\t";
                        emit (ID.toString (#name sym));
                        emit ",";
                        emit (I.toString (siz div 8));
                        emit ",4";
                        newline ()
                    end
              | _ => 
                    (emitVarPrelude (sym, alignment div 8);
                     case valueBlock of
                         SOME vb => compileValueBlock (vb, siz)
                       | NONE => ())
        end

    fun emitProcPrelude (sym as {uid = _, name = nm}) =
        let
            val label = newLabel (SOME sym, true)
            val procName = ID.toString nm
            val _ = print ("Compiling procedure " ^ procName ^ "\n")
        in
            textSection ();
            align 8;
            emitgDecl procName;
            emitLabel label;
            emitProcDecl procName
        end

    fun compileLocalVars (params, locals : bool) =
        let
            val frameOffset = ref 68

            fun compileParam {symbol = Z.ParameterEntry
                              {key = key, address_taken = addressTaken,
                               name = {uid = _, name = varName},
                               bit_alignment = bitAlignment,
                               type' = type_id, proc = proc},
                              refname = refname} = 
                let
                    val name = ID.toString varName
                    val regType = typIdtoRegty type_id
                    val regSize = regTytoSize regType
                    val loc = Loc (!argNum, true)

                    val f1 = "d." ^ (I.toString (!procNum)) ^ "_" ^ name
                    val f2 = locToString loc
                    val f3 = paramLevel
                    val f4 = regTytoString regType
                    val f5 = I.toString regSize
                    val f6 = if locals then "" else I.toString (!frameOffset)
                    val f7 = ""                                  (* For now *)
                in
                    emit f1; tab ();
                    emit f2; tab ();
                    emit f3; tab ();
                    emit f4; tab ();
                    emit f5; tab ();
                    emit f6; tab ();
                    emit f7; newline ();
                    argNum      := !argNum + 1;
                    frameOffset := !frameOffset + regSize;
                    refname     := SOME loc
                end
              | compileParam _ = raise BadParameterInFunctionParamList
        in
            app (compileParam o findSymbol) params;
            !frameOffset
        end

    fun zeroOut reg =
        (newstat (); emitReg reg; emit "=0"; newline ())

    fun addOne reg =
        (newstat (); emitReg reg; emit "=";
         emitReg reg; emit "+1"; newline ())

    fun constToReg (Z.ConstInt (Z.Finite n), reg) =
        let
            val reprInt = 4096
            val si      = I.toString n
        in
            if n >= reprInt orelse n < ~reprInt then
                (newstat ();
                 emitReg reg; emit "=HI["; emit si; emit "]\n";
                 newstat ();
                 emitReg reg; emit "="; emitReg reg; emit "|LO[";
                 emit si; emit "]")
            else
                (newstat (); emitReg reg; emit "="; emit si);
                newline ()
        end

      | constToReg (Z.ConstString str, reg) =
        let
            val newl = newLabel (NONE, false)
            val labname = getLabName newl
            val (algn, emitfn, deref)
                = case reg of
                (Reg (Fp32Bit, _)) => (4, emitSingle, "F")
              | (Reg (Fp64Bit, _)) => (8, emitDouble, "D")
              | _ => raise (Fail "Bad reg in constToReg")
            val addrReg = newReg Int32Bit
        in
            dataSection ();
            align algn;
            emitfn (str, newl);
            textSection ();
            newstat ();
            emitReg addrReg; emit "=HI["; emit labname; emit "]";
            newline ();
            newstat ();
            emitReg reg; emit "="; emit deref; emit "[";
            emitReg addrReg; emit "+"; emit "LO["; emit labname; emit "]]";
            newline ()
        end
      | constToReg _ = raise Can'tDoItYet

    val glo = ref 0

    exception VarAddressRegProblem

    fun emitVarDef (name, loc, typ, is_local) =
        let
            val regType = #1(getRegType typ)
            val regSize = regTytoSize regType
            val tmps = if is_local
                       then "." ^ (I.toString (!procNum)) ^ "_"
                       else ""

            val f1 = "d" ^ tmps ^ name
            val (f2, f3) = if is_local
                           then (locToString loc, localLevel)
                           else (gloToString loc, globalLevel)
            val f4 = regTytoString regType
            val f5 = if is_local then I.toString regSize else ""
        in
            emit f1; tab ();
            emit f2; tab ();
            emit f3; tab ();
            emit f4; tab ();
            emit f5;
            newline ()
        end

    fun varAddressReg (ste as {symbol = sym, refname = r as (ref NONE)}) =
        (case sym of
             Z.VariableEntry {key = key,
                              address_taken = _,
                              def = {name = vsym,
                                     type' = typ,
                                     value_block = _},
                              is_local = is_local} =>
             let
                 val name = ID.toString (#name vsym)
                 val loc  = if is_local then
                      Loc (!argNum, false) before argNum := !argNum + 1
                            else
                      Glo (!glo) before glo := !glo + 1
             in
                 emitVarDef (name, loc, typ, is_local);
                 r := SOME loc;
                 varAddressReg ste
             end
 
           | Z.ProcedureEntry {key = _, address_taken = _,
                               def = {name = psym,
                                      qualifications =_,
                                      procedure_type = procTyp,
                                      procedure_body =_}} =>
             let
                 val loc  = Glo (!glo) before glo := !glo + 1
                 val name = ID.toString (#name psym)
                 val typ  = Z.Procedure procTyp
             in
                 emitVarDef (name, loc, typ, false);
                 r := SOME loc;
                 varAddressReg ste
             end

           | _ => raise VarAddressRegProblem)

      | varAddressReg {symbol = sym, refname = r as (ref (SOME st))} =
        let
            val regtmp  = newReg Int32Bit
        in
            case st of
                Loc (_, is_local) => (newstat ();
                          emitReg regtmp;
                          emit "=";
                          emitReg (if is_local then fp else sp);
                          emit "+";
                          emitLoc st;
                          newline ())
              | Glo n => (newstat ();
                          emitReg regtmp;
                          emit "=HI[";
                          emitGlo st;
                          emit "]\n";
                          newstat ();
                          emitReg regtmp;
                          emit "=";
                          emitReg regtmp;
                          emit "+LO[";
                          emitGlo st;
                          emit "]\n")
              | _     => raise BadLocation;
            regtmp
        end

    fun emitUncondJump (lab) =
        (newstat (); emit "PC="; emit (getLabName lab); newline())

    local
	val sEmited = ref false
    in
	fun initSEmited () = sEmited := false
	fun emitSaveSt reg =
	    if not (!sEmited) then
		(emit "s="; emitReg reg; emit ";"; newline ();
		 sEmited := true)
	    else ()
    end

    fun emitReturnSt () =
        (newstat (); emit "PC=RT"; newline ())

    fun emitAssignmentLDeref (regt, regs) =
        let
            val left = toUpper (regToLetter regs)
        in
            newstat ();
            emit left; emit "["; emitReg regt; emit "]";
            emit "="; emitReg regs; newline ();
            regt
        end

    fun emitAssignmentRDeref (regt, regs) =
        let
            val left = toUpper (regToLetter regt)
        in
            newstat ();
            emitReg regt; emit "=";
            emit left; emit "["; emitReg regs; emit "]"; newline ();
            regt
        end

    fun getCondCode reg =
        case reg of
            (Reg (Fp32Bit, _)) => "FC"
          | (Reg (Fp64Bit, _)) => "FC"
          | (Reg _)            => "IC"
          | _                  => raise (Fail "Not a reg in emitICSt2")

    fun emitICSt2 (reg1, reg2) =
        let
            val condCode = getCondCode reg1
        in
            newstat ();
            emit condCode; emit "=";
            emitReg reg1; emit "?"; emitReg reg2;
            newline ()
        end

    fun emitICSt reg =
        let
            val zreg = newReg Int32Bit
        in
            zeroOut zreg;
            emitICSt2 (reg, zreg)
        end

    fun emitJumpIfZero (res, lab) =
        (emitICSt res;
         newstat (); emit "PC=IC:0,"; emit (getLabName lab); newline())

    fun emitJumpIfNotZero (res, lab) =
        (emitICSt res;
         newstat (); emit "PC=IC!0,"; emit (getLabName lab); newline())

    fun cSourceOp (Z.SrcVar {var = var}) =
        let
            val ste as {symbol = sym, refname = refname} = findSymbol var
            val addrReg = varAddressReg ste
            val typ     = regTypeSte sym
            val valReg  = newReg typ
        in
            emitAssignmentRDeref (valReg, addrReg)
        end

      | cSourceOp (Z.SrcReg {reg = reg, type' = typ}) =
            raise ShouldNotBeUsed

      | cSourceOp (Z.SrcDst {instr = instr, op_num = opNum}) =
        let
            val resList = cInstruction instr
            val reg = L.nth (resList, opNum)
        in
            reg
        end

      | cSourceOp (Z.SrcZero) =
        let
            val reg = newReg Int32Bit
        in
            newstat (); emitReg reg; emit "=0"; newline ();
            reg
        end

    and compToRtl (Z.Is_equal_to)                 = "!"
      | compToRtl (Z.Is_not_equal_to)             = ":"
      | compToRtl (Z.Is_less_than)                = "`"
      | compToRtl (Z.Is_less_than_or_equal_to)    = ">"
      | compToRtl (Z.Is_greater_than)             = "'"
      | compToRtl (Z.Is_greater_than_or_equal_to) = "<"
      | compToRtl _                               =
        raise (Fail "Not a comparison operator : compToRtl")

    and doCompOper (res, reg1, oper, reg2) =
        let
            val lab = newLabel (NONE, false)
            val condCode = getCondCode reg1
        in
            zeroOut res;
            emitICSt2 (reg1, reg2);
            newstat ();
            emit "PC="; emit condCode;
            emit (compToRtl oper); emit "0,";
            emit (getLabName lab); newline ();
            addOne res;
            emitLabel lab
        end

    and cDestOp (Z.DstTmp, src) = src

      | cDestOp (Z.DstVar {var = var}, src) =
        let
            val ste as {symbol = sym, refname = refname} = findSymbol var
            val addrReg = varAddressReg ste
        in
            emitAssignmentLDeref (addrReg, src);
            src
        end

      | cDestOp (Z.DstReg {reg = reg, type' = type'}, res) =
        raise ShouldNotBeUsed

      | cDestOp (Z.DstSrc {instr = instr, op_num = opNum}, src) =
        let
            val addrReg = L.nth (cInstruction instr, opNum)
        in
            emitAssignmentLDeref (addrReg, src);
            src
        end

    and builtinUoper (regt, oper, reg) =
        (newstat (); emitReg regt; emit oper; emitReg reg; newline ())

    and cUnaryOperator (Z.Negate, reg, res) =
        builtinUoper (res, "-", reg)

      | cUnaryOperator (Z.Invert, reg, res) = raise Can'tDoItYet

      | cUnaryOperator (Z.Absolute_value, reg, res) =
	let
	    val lab = newLabel (NONE, false)
	in
	    raise Can'tDoItYet
	end

      | cUnaryOperator (Z.Bitwise_not, reg, res) =
	cBinOperator (Z.Bitwise_nand, reg, reg, res)

      | cUnaryOperator (Z.Logical_not, reg, res) =
        let
            val lab = newLabel (NONE, false)
        in
            zeroOut res;
            emitJumpIfNotZero (reg, lab);
            addOne res;
            emitLabel lab
        end

      | cUnaryOperator (Z.Convert, reg, res) = 
        let
            fun same() = regAssign (res, reg, false)
            val _ =
                case reg of
                    (Reg (x, _)) => print ("Reg is " ^ (regTytoName x) ^ "\n")
                  | _            => raise (Fail "")
            val _ = 
                case res of
                    (Reg (x, _)) => print ("Res is " ^ (regTytoName x) ^ "\n")
                  | _            => raise (Fail "")
        in
            case (reg, res) of
                (Reg (Int8Bit, _),  Reg (Int8Bit, _))  => same ()
              | (Reg (Int16Bit, _), Reg (Int16Bit, _)) => same ()
              | (Reg (Int32Bit, _), Reg (Int32Bit, _)) => same ()
              | (Reg (Fp32Bit, _),  Reg (Fp32Bit, _))  => same ()
              | (Reg (Fp64Bit, _),  Reg (Fp64Bit, _))  => same ()
              | _                                      => raise Can'tDoItYet
        end
                                        (* Needs more work *)
      | cUnaryOperator (Z.Treat_as, reg, res) =
        raise Can'tDoItYet

    and builtinOper (regt, reg1, oper, reg2) =
        (newstat ();
         emitReg regt; emit "="; emitReg reg1; emit oper; emitReg reg2;
         newline ())

    and cMulDivRem (regt, reg1, oper, reg2) =
        let
            val (realRegs, _) = doArgs [reg1, reg2]
            val (grel, name) = case oper of
                                    Mul => (multi, "mul")
                                  | Div => (divid, "div")
                                  | Rem => (remen, "rem")
	    val output = ref true
            val (global, seenBefore) =
		case !grel of
		    NONE => let
                        val g = Glo (!glo)
		    in
                        grel := SOME g; glo := !glo + 1; (g, false)
                    end
		  | SOME x => (x, true)
        in
            if not seenBefore then
		(emit "d."; emit name; tab (); emitGlo global;
		 tab (); emit "0"; tab (); emit "2\n")
	    else ();
            newstat ();
            emitReg r8; emit "=UC["; emitGlo global;
            app (fn r => (emit ",";emitReg r)) realRegs; emit "]\n";
            emitU ();
            emitS ();
            regAssign (regt, r8, false)
        end

    and cBinOperator (Z.Add, reg1, reg2, res) =
        builtinOper (res, reg1, "+", reg2)

      | cBinOperator (Z.Subtract, reg1, reg2, res) =
        builtinOper (res, reg1, "-", reg2)

      | cBinOperator (Z.Multiply, reg1 as (Reg (Fp32Bit, _)),
                                  reg2 as (Reg (Fp32Bit, _)), res) =
        builtinOper (res, reg1, "*", reg2)

      | cBinOperator (Z.Multiply, reg1 as (Reg (Fp64Bit, _)),
                                  reg2 as (Reg (Fp64Bit, _)), res) =
        builtinOper (res, reg1, "*", reg2)

      | cBinOperator (Z.Multiply, reg1, reg2, res) =
        cMulDivRem (res, reg1, Mul, reg2)

      | cBinOperator (Z.Divide, reg1 as (Reg (Fp32Bit, _)),
                                reg2 as (Reg (Fp32Bit, _)), res) =
        builtinOper (res, reg1, "/", reg2)

      | cBinOperator (Z.Divide, reg1 as (Reg (Fp64Bit, _)),
                                reg2 as (Reg (Fp64Bit, _)), res) =
        builtinOper (res, reg1, "/", reg2)

      | cBinOperator (Z.Divide, reg1, reg2, res) =
        cMulDivRem (res, reg1, Div, reg2)

      | cBinOperator (Z.Remainder, reg1, reg2, res) =
        cMulDivRem (res, reg1, Rem, reg2)

      | cBinOperator (Z.Bitwise_and, reg1, reg2, res) =
        builtinOper (res, reg1, "&", reg2)

      | cBinOperator (Z.Bitwise_or, reg1, reg2, res) =
        builtinOper (res, reg1, "|", reg2)

      | cBinOperator (Z.Bitwise_nand, reg1, reg2, res) =
        builtinOper (res, reg1, "b", reg2)

      | cBinOperator (Z.Bitwise_nor, reg1, reg2, res) =
        builtinOper (res, reg1, "o", reg2)

      | cBinOperator (Z.Bitwise_xor, reg1, reg2, res) =
        builtinOper (res, reg1, "^", reg2)

      | cBinOperator (Z.Left_shift, reg1, reg2, res) =
        builtinOper (res, reg1, "{", reg2)

      | cBinOperator (Z.Right_shift, reg1, reg2, res) =
        builtinOper (res, reg1, "}", reg2)

      | cBinOperator (Z.Rotate, reg1, reg2, res) =
	raise Can'tDoItYet

      | cBinOperator (x as Z.Is_equal_to, reg1, reg2, res) =
        doCompOper (res, reg1, x, reg2)

      | cBinOperator (x as Z.Is_not_equal_to, reg1, reg2, res) =
        doCompOper (res, reg1, x, reg2)

      | cBinOperator (x as Z.Is_less_than, reg1, reg2, res) =
        doCompOper (res, reg1, x, reg2)

      | cBinOperator (x as Z.Is_less_than_or_equal_to, reg1, reg2, res) =
        doCompOper (res, reg1, x, reg2)

      | cBinOperator (x as Z.Is_greater_than, reg1, reg2, res) =
        doCompOper (res, reg1, x, reg2)

      | cBinOperator (x as Z.Is_greater_than_or_equal_to, reg1, reg2, res) =
        doCompOper (res, reg1, x, reg2)

      | cBinOperator (Z.Logical_and, reg1, reg2, res) =
        let
            val lab = newLabel (NONE, false)
        in
            zeroOut res;
            emitJumpIfZero (reg1, lab);
            emitJumpIfZero (reg2, lab);
            addOne res;
            emitLabel lab
        end

      | cBinOperator (Z.Logical_or, reg1, reg2, res) =
        let
            val lab = newLabel (NONE, false)
        in
            zeroOut res;
            addOne res;
            emitJumpIfNotZero (reg1, lab);
            emitJumpIfNotZero (reg2, lab);
            zeroOut res;
            emitLabel lab
        end

      | cBinOperator (Z.Maximum, reg1, reg2, res) =
        let
            val lab1 = newLabel (NONE, false)
            val lab2 = newLabel (NONE, false)
        in
            emitICSt2 (reg1, reg2);
            newstat (); emit "PC=IC'0,"; emit (getLabName lab1); newline ();
            regAssign (res, reg1, false);
            emitUncondJump lab2;
            emitLabel lab1;
            regAssign (res, reg2, false);
            emitLabel lab2
        end

      | cBinOperator (Z.Minimum, reg1, reg2, res) =
        let
            val lab1 = newLabel (NONE, false)
            val lab2 = newLabel (NONE, false)
         in
            emitICSt2 (reg1, reg2);
            newstat (); emit "PC=IC`0,"; emit (getLabName lab1); newline ();
            regAssign (res, reg1, false);
            emitUncondJump lab2;
            emitLabel lab1;
            regAssign (res, reg2, false);
            emitLabel lab2
        end

    and cBinaryArithInstruction {binop = binop,
                                 source1 = source1,
                                 source2 = source2,
                                 result_type = resultTyp, (* Needs more work *)
                                 destination_op = destOp} =
        let
            val reg1   = cSourceOp source1
            val reg2   = cSourceOp source2
            val regTyp = typIdtoRegty resultTyp
            val res    = newReg regTyp
        in
            cBinOperator (binop, reg1, reg2, res);
            [cDestOp (destOp, res)]
        end

    and cUnaryArithInstruction {unop = unop,
                                source = source,
                                result_type = resultTyp,
                                destination_op = destOp} =
        let
            val reg    = cSourceOp source
            val regtyp = typIdtoRegty resultTyp
            val res    = newReg regtyp
        in
            cUnaryOperator (unop, reg, res);
            [cDestOp (destOp, res)]
        end

    and cCopyInstruction {source = source,
                          result_type = resultTyp,
                          destination_op = destOp,
                          destination_ops = destOps} =
	let
	    val res = cSourceOp source
	in
	    [cDestOp (destOp, res)]
	end

    and cSelectInstruction {selector = selector,
                            selection1 = selection1,
                            selection2 = selection2,
                            result_type = resultTyp,
                            destination_op = destOp} =
	let
	    val selReg    = cSourceOp selector
	    val regtyp    = typIdtoRegty resultTyp
	    val res       = newReg regtyp
	    val lab1      = newLabel (NONE, false)
	    val lab2      = newLabel (NONE, false)
	in
	    emitJumpIfZero (selReg, lab1);
	    regAssign (res, cSourceOp selection1, false);
	    emitUncondJump lab2;
	    emitLabel lab1;
	    regAssign (res, cSourceOp selection2, false);
	    emitLabel lab2;
	    [cDestOp (destOp, res)]
	end

    and cArrayRefInstruction {base_array_address = baseArrayAddress,
                              index = index,
                              result_type = resultTyp,
                              destination_op = destOp} =
        let
            val baseReg       = cSourceOp baseArrayAddress
            val indexReg      = cSourceOp index
            val typ           = findType resultTyp
            val siz           = #2 (getRegType (#value typ))

            val sizReg   = newReg Int32Bit
            val addrReg  = newReg Int32Bit
        in
            constToReg (Z.ConstInt (Z.Finite siz), sizReg);
            cBinOperator(Z.Multiply, sizReg, indexReg, addrReg);
            cBinOperator(Z.Add, baseReg, addrReg, addrReg);
            [cDestOp (destOp, addrReg)]
        end

    and cFieldAccInstruction {base_group_address = baseGroupAddress,
                              field = field,
                              result_type = resultTyp,
                              destination_op = destOp} =
        let
            fun getsiz iors =
                case iors of
                    Z.Int n => n div 8
                  | _ => raise (Fail "Bad size for element offset")

            val {key = key, address_taken = _,
                 bit_offset = bitOffset} =
                case #symbol (findSymbol field) of
                    Z.FieldEntry x => x
                  | _              => raise (Fail "Illegal field object")

            val baseAdrReg = cSourceOp baseGroupAddress
            val typ        = findType resultTyp
            val resTyp     = #1 (getRegType (#value typ))
            val siz        = getsiz bitOffset
            val res        = newReg resTyp
            val offsetReg  = newReg Int32Bit
        in
            constToReg (Z.ConstInt (Z.Finite siz), offsetReg);
            cBinOperator(Z.Add, baseAdrReg, offsetReg, baseAdrReg);
            [cDestOp (destOp, baseAdrReg)]
        end

    and cExtractFieldsInstructrion {base_group_op = baseGroupOp,
                                   field_dst = fieldDst,
                                   field_dsts = fieldDsts} =
        raise Can'tDoItYet

    and cSetFieldsInstruction {base_group_op = baseGroupOp,
                               field_src = fieldSrc,
                               field_srcs = fieldSrcs,
                               result_type = resultTyp,
                               destination_op = destOp} =
        raise Can'tDoItYet

    and cExtractElemInstruction {base_array_op = baseArrayOp,
                                 element_dst = elementDst,
                                 element_dsts = elementDsts} =
        raise Can'tDoItYet

    and cSetElemInstruction {base_array_op = baseArrayOp,
                             element_src = elementSrc,
                             element_srcs = elementSrcs} =
        raise Can'tDoItYet

    and cBitSizeOfInstruction {ref_type = refTyp,
                               result_type = resultTyp,
                               destination_op = destOp} =
        raise Can'tDoItYet

    and cBitAlignmentOfInstruction {ref_type = refTyp,
                                    result_type = resultTyp,
                                    destination_op = destOp} =
        raise Can'tDoItYet

    and cBitOffsetOfInstruction {field = field,
                                 result_type = resultTyp,
                                 destination_op = destOp} =
        raise Can'tDoItYet
    and cByteSizeOfInstruction{ref_type = refTyp,
                               result_type = resultTyp,
                               destination_op = destOp} =
        raise Can'tDoItYet

    and cByteAlignmentOfInstruction{ref_type = refTyp,
                                    result_type = resultTyp,
                                    destination_op = destOp} =
        raise Can'tDoItYet

    and cByteOffsetOfInstruction {field = field,
                                  result_type = resultTyp,
                                  destination_op = destOp} =
        raise Can'tDoItYet

    and cVaArgInstruction {ap_address = apAddress,
                           result_type = resultTyp,
                           destination_op = destOp} =
        raise Can'tDoItYet

    and cScAndInstruction {source1 = source1,
                           source2 = source2,
                           result_type = resultTyp,
                           destination_op = destOp} =
        raise Can'tDoItYet

    and cScOrInstruction{source1 = source1,
                         source2 = source2,
                         result_type = resultTyp,
                         destination_op = destOp} =
        raise Can'tDoItYet

    and cScSelectInstruction {selector = selector,
                              selection1 = selection1,
                              selection2 = selection2,
                              result_type = resultTyp,
                              destination_op = destOp} =
        raise Can'tDoItYet

    and cLoadInstruction {source_address = sourceAddress,
                          result_type = resultTyp,
                          destination_op = destOp} =
        let
            val addrReg = cSourceOp sourceAddress
            val resTyp  = typIdtoRegty resultTyp
            val res     = newReg resTyp
        in
            emitAssignmentRDeref(res, addrReg);
            [cDestOp (destOp, res)]
        end

    and cLoadAddressInstruction {addressed_symbol = var,
                                 result_type = resultTyp,
                                 destination_op = destOp} =
        let
            val ste as {symbol = sym, refname = refname} = findSymbol var
            val addrReg = varAddressReg ste
        in
            [cDestOp(destOp, addrReg)]
        end

    and cLoadConstantInstruction {constant = constant,
                                  result_type = resultTyp,
                                  destination_op = destOp} =
        let
            val regTyp = case destOp of
                             (Z.DstVar {var = v}) =>
                                 let
                                     val {symbol = sym, refname = _} =
                                         findSymbol v
                                 in
                                     regTypeSte sym
                                 end
                           | _ => typIdtoRegty resultTyp
            val reg = newReg regTyp
        in
            constToReg (constant, reg);
            [cDestOp (destOp, reg)]
        end

    and cLoadValueBlockInstruction {constant = constant,
                                    result_type = resultTyp,
                                    destination_op = destOp} =
        raise Can'tDoItYet


    and cCallInstruction {callee_address = calleeAddress,
                          arguments = arguments,
                          return_values = returnValues} =
        let
            fun h () =
                let
                    val fnAddrReg         = cSourceOp calleeAddress
                    val argRegs           = map cSourceOp arguments
                    val (realRegs, depth) = doArgs argRegs
                    val argsNum           = I.toString (length realRegs)
                in
                    newstat ();
                    emit "ST="; emitReg fnAddrReg;
                    emit ","; emit (I.toString depth);
                    emit ","; emit argsNum;
                    emit "\tICFC"; emitReg fnAddrReg;
                    newline ();
                    emitC ();
                    newstat (); emit "\t"; app emitReg realRegs; newline ();
                    emitU ();
                    emitS ()
                end

            fun ddst (destOp, res) = [cDestOp (destOp, res)]
        in
            case returnValues of
                (retVal :: _) =>
                    let
                        val destOp    = #destination_op retVal
                        val type_id   = #result_type retVal
                        val typ       = findType type_id
                        val resTyp    = #1 (getRegType (#value typ))
                        val res       = newReg resTyp
                        val resultReg = case resTyp of
                                            Fp32Bit  => f0
                                          | Fp64Bit  => d0
                                          | Int32Bit => r8
                                          | Int16Bit => w8
                                          | Int8Bit  => b8
                    in
                        h ();
                        emit "r"; emitReg res; emit "="; emitReg resultReg;
                        newline ();
                        ddst (destOp, res)
                    end
              | [] => (h (); ddst (Z.DstTmp, r0))
        end

    and cSsaPhiInstruction {variables = variables,
                            result_type = resultTyp,
                            destination_op = destOp} =
        raise Can'tDoItYet

    and cMarkInstruction () =
        raise Can'tDoItYet

    and cVaStartInstruction {ap_address = apAddress,
                             parmn = parmn} =
        raise Can'tDoItYet

    and cVaStartOldInstruction {ap_address = apAddress} =
        raise Can'tDoItYet

    and cVaEndInstruction {ap_address = apAddress} =
        raise Can'tDoItYet

    and cStoreInstruction {data_operand = dataOperand,
                           destination_address = destAddress} =
        raise Can'tDoItYet

    and cReturnInstruction {return_values = returnVals} =
        raise Can'tDoItYet

    and cJumpInstruction {target = target} =
        raise Can'tDoItYet

    and cJumpIndirectInstruction {target = target} =
        raise Can'tDoItYet

    and cBranchTrueInstruction {decision_operand = decisionOper,
                                target = target} =
        raise Can'tDoItYet

    and cBranchFalseInstruction {decision_operand = decisionOper,
                                 target = target} =
        raise Can'tDoItYet

    and cMultiWayBranchInstruction {decision_operand = decisionOper,
                                    default_target = defaultTarget,
                                    cases = cases} =
        raise Can'tDoItYet
    and cLabelLocationInstruction {defined_label = definedLabel} =
        raise Can'tDoItYet

    and cAssertInstruction {asserted_value = assertedVal} =
        raise Can'tDoItYet

    and cInstruction (Z.Binary_arithmetic_instruction instr) =
        cBinaryArithInstruction instr

      | cInstruction (Z.Unary_arithmetic_instruction instr) =
        cUnaryArithInstruction instr

      | cInstruction (Z.Copy_instruction instr) =
        cCopyInstruction instr

      | cInstruction (Z.Select_instruction instr) =
        cSelectInstruction instr

      | cInstruction (Z.Array_reference_instruction instr) =
        cArrayRefInstruction instr

      | cInstruction (Z.Field_access_instruction instr) =
        cFieldAccInstruction instr

      | cInstruction (Z.Extract_fields_instruction instr) =
        cExtractFieldsInstructrion instr

      | cInstruction (Z.Set_fields_instruction instr) =
        cSetFieldsInstruction instr

      | cInstruction (Z.Extract_elements_instruction instr) =
        cExtractElemInstruction instr

      | cInstruction (Z.Set_elements_instruction instr) =
        cSetElemInstruction instr

      | cInstruction (Z.Bit_size_of_instruction instr) =
        cBitSizeOfInstruction instr

      | cInstruction (Z.Bit_alignment_of_instruction instr) =
        cBitAlignmentOfInstruction instr

      | cInstruction (Z.Bit_offset_of_instruction instr) =
        cBitOffsetOfInstruction instr

      | cInstruction (Z.Byte_size_of_instruction instr) =
        cByteSizeOfInstruction instr

      | cInstruction (Z.Byte_alignment_of_instruction instr) =
        cByteAlignmentOfInstruction instr

      | cInstruction (Z.Byte_offset_of_instruction instr) =
        cByteOffsetOfInstruction instr

      | cInstruction (Z.Va_arg_instruction instr) =
        cVaArgInstruction instr

      | cInstruction (Z.Sc_and_instruction instr) =
        cScAndInstruction instr

      | cInstruction (Z.Sc_or_instruction instr) =
        cScOrInstruction instr

      | cInstruction (Z.Sc_select_instruction instr) =
        cScSelectInstruction instr

      | cInstruction (Z.Load_instruction instr) =
        cLoadInstruction instr

      | cInstruction (Z.Load_address_instruction instr) =
        cLoadAddressInstruction instr

      | cInstruction (Z.Load_constant_instruction instr) =
        cLoadConstantInstruction instr

      | cInstruction (Z.Load_value_block_instruction instr) =
        cLoadValueBlockInstruction instr

      | cInstruction (Z.Call_instruction instr) =
        cCallInstruction instr

      | cInstruction (Z.Ssa_phi_instruction instr) =
        cSsaPhiInstruction instr

      | cInstruction (Z.Mark_instruction) =
        cMarkInstruction ()

      | cInstruction (Z.Va_start_instruction instr) =
        cVaStartInstruction instr

      | cInstruction (Z.Va_start_old_instruction instr) =
        cVaStartOldInstruction instr

      | cInstruction (Z.Va_end_instruction instr) =
        cVaEndInstruction instr

      | cInstruction (Z.Store_instruction instr) =
        cStoreInstruction instr

      | cInstruction (Z.Return_instruction instr) =
        cReturnInstruction instr

      | cInstruction (Z.Jump_instruction instr) =
        cJumpInstruction instr

      | cInstruction (Z.Jump_indirect_instruction instr) =
        cJumpIndirectInstruction instr

      | cInstruction (Z.Branch_true_instruction instr) =
        cBranchTrueInstruction instr

      | cInstruction (Z.Branch_false_instruction instr) =
        cBranchFalseInstruction instr

      | cInstruction (Z.Multi_way_branch_instruction instr) =
        cMultiWayBranchInstruction instr

      | cInstruction (Z.Label_location_instruction instr) =
        cLabelLocationInstruction instr

      | cInstruction (Z.Assert_instruction instr) =
        cAssertInstruction instr


    exception EmptyInstructionList

    fun compileEval {instructions = []} = raise EmptyInstructionList

      | compileEval {instructions = instructions} =
        app (ignore o cInstruction) instructions

    and compileSequence {statements = statements} =
        app compileStatement statements

    and compileIfThenElse {condition = condition,
                           then_part = thenPart,
                           else_part = elsePart} =
        let
            val lab1 = newLabel (NONE, false)
            val lab2 = newLabel (NONE, false)
            val res  = cSourceOp condition
        in
            emitJumpIfZero (res, lab1);
            compileStatement thenPart;
            emitUncondJump lab2;
            emitLabel lab1;
            compileStatement elsePart;
            emitLabel lab2
        end

    and compileWhile {condition = condition,
                      body = body,
                      break_label = breakLabOpt, (* Check this later *)
                      continue_label = contLabOpt} = (* Check this later *)
        let
            val beginLab = newLabel (contLabOpt, false)
            val endLab   = newLabel (breakLabOpt, false)
            val res      = cSourceOp condition
        in
            emitLabel beginLab;
            emitJumpIfZero (res, endLab);
            compileStatement body;
            emitUncondJump beginLab;
            emitLabel endLab
        end

    and compileDoWhile {condition = condition,
                        body = body,
                        break_label = breakLabOpt,
                        continue_label = contLabOpt} =
        let
            val beginLab    = newLabel (NONE, false)
            val continueLab = newLabel (contLabOpt, false)
            val breakLab    = newLabel (breakLabOpt, false)
        in
            emitLabel beginLab;
            compileStatement body;
            emitLabel continueLab;
            let
                val res = cSourceOp condition
            in
                emitJumpIfNotZero (res, beginLab);
                emitLabel breakLab
            end
        end

    and compileForStatement {index = indexVar : Z.symbol,
                             lower_bound = lowerBound,
                             upper_bound = upperBound,
                             step = step,
                             init_comparison_opcode = initCompOpcode,
                             body = fbody,
                             pre_pad = prePadOpt,
                             post_pad = postPadOpt,
                             break_label = breakLabOpt,
                             continue_label = contLabOpt} =
        let
            val continueLab = newLabel (contLabOpt, false)
            val breakLab    = newLabel (breakLabOpt, false)

            val ste as {symbol = sym, refname = refname} = findSymbol indexVar
        in
            case prePadOpt of
                NONE => ()
              | SOME st => compileStatement st;
            let
                val ireg = varAddressReg ste
                val vreg = cSourceOp lowerBound
            in
                emitAssignmentLDeref (ireg, vreg)
            end;

            emitLabel continueLab;

            let
                val indexReg = cSourceOp (Z.SrcVar {var = indexVar})
                val ureg     = cSourceOp upperBound
                val regTyp   = regTypeSte sym
                val res      = newReg regTyp
            in
                cBinOperator (initCompOpcode, indexReg, ureg, res);
                emitJumpIfZero (res, breakLab)
            end;

            compileStatement fbody;

            let
                val ireg = varAddressReg ste
                val indexReg = cSourceOp (Z.SrcVar {var = indexVar})
                val stReg = cSourceOp step
            in
                cBinOperator (Z.Add, indexReg, stReg, indexReg);
                emitAssignmentLDeref (ireg, indexReg)
            end;

            emitUncondJump continueLab;

            emitLabel breakLab;

            case postPadOpt of
                NONE => ()
              | SOME st => compileStatement st
        end
                
    and compileScopeStatement {body = sbody,
                               definition_block = definitionBlock} =
        let
            val {defined_variables = definedVars,
                 defined_procedures = _} = definitionBlock
        in
                                        (* These are not local but static *)
                                        (* Needs to be fixed and tested! *)
            compileLocalVars(definedVars, true);
            compileStatement sbody
        end

    and compileMarkStatement () = ()    (* Nothing to do here *)

    and compileVaStartStatement {ap_address = apAddress, parmn = paramSym} =
        raise Can'tDoItYet

    and compileVaStartOldStatement {ap_address = apAddress} =
        raise Can'tDoItYet

    and compileVaEndStatement {ap_address = apAddress} =
        raise Can'tDoItYet

    and compileStoreStatement {data_operand = dataOperand,
                               destination_address = destAddress} =
        let
            val dataReg = cSourceOp dataOperand
            val destReg = cSourceOp destAddress
        in
            ignore (emitAssignmentLDeref (destReg, dataReg))
        end

    and compileReturnStatement {return_values = []} = emitReturnSt ()

      | compileReturnStatement {return_values = returnValuesList} =
        let                   (* We only compile the  first value for now *)
            val cOp    = hd returnValuesList
            val resReg = cSourceOp cOp
            val (regtyp, regn, restore) =
                case resReg of
                    (Reg (Fp32Bit, _)) => (Fp32Bit, 0, false)
                  | (Reg (Fp64Bit, _)) => (Fp64Bit, 0, false)
                  | (Reg (rtyp, _))    => (rtyp, 8, true)
                  | _                  => raise (Fail "Not a reg in returnst");
            val retReg = newFixedReg (regtyp, regn)
        in
            regAssign (retReg, resReg, restore);
            if restore then () else useReg retReg;
            emitSaveSt retReg;
            emitReturnSt ()
        end

    and compileJumpStatement {target = target} =
        let
            val lab = newLabel (SOME target, false);
        in
            emitUncondJump lab
        end

    and compileJumpIndirectStatement {itarget = itarget} =
        raise Can'tDoItYet

    and compileBranchTrueStatement {decision_operand = decisionOperand,
                                    target = target} =
        let
            val lab = newLabel (SOME target, false)
            val reg = cSourceOp decisionOperand
        in
            emitJumpIfNotZero(reg, lab)
        end
    and compileBranchFalseStatement {decision_operand = decisionOperand,
                                     target = target} =
        let
            val lab = newLabel (SOME target, false)
            val reg = cSourceOp decisionOperand
        in
            emitJumpIfZero(reg, lab)
        end

    and compileMultiWayBranchStatement {decision_operand = decisionOperand,
                                        default_target = defaultTargetLabel,
                                        cases = casesList} =
        raise Can'tDoItYet              (* Needs more work *)

    and compileLabelLocationStatement {defined_label = definedLabel} =
        let
            val lab = newLabel (SOME definedLabel, false)
        in
            emitLabel lab
        end

    and compileAssertStatement {asserted_value = assertedValue} =
        raise Can'tDoItYet              (* Needs more work *)

    and compileNopStatement () = ()    (* Nothing to do here *)

    and compileStatement body =
        case body of
            Z.Eval_statement instrs =>
                compileEval instrs

          | Z.Sequence_statement stats =>
                compileSequence stats

          | Z.If_statement ifte =>
                compileIfThenElse ifte

          | Z.While_statement wst =>
                compileWhile wst

          | Z.Do_while_statement dwst =>
                compileDoWhile dwst

          | Z.For_statement fst =>
                compileForStatement fst

          | Z.Scope_statement scst =>
                compileScopeStatement scst

          | Z.Mark_statement =>
                compileMarkStatement ()

          | Z.Va_start_statement vast =>
                compileVaStartStatement vast

          | Z.Va_start_old_statement vaost =>
                compileVaStartOldStatement vaost

          | Z.Va_end_statement vaest =>
                compileVaEndStatement vaest

          | Z.Store_statement stost =>
                compileStoreStatement stost

          | Z.Return_statement rest =>
                compileReturnStatement rest

          | Z.Jump_statement just =>
                compileJumpStatement just

          | Z.Jump_indirect_statement jist =>
                compileJumpIndirectStatement jist

          | Z.Branch_true_statement btst =>
                compileBranchTrueStatement btst

          | Z.Branch_false_statement bfst =>
                compileBranchFalseStatement bfst

          | Z.Multi_way_branch_statement mwbst =>
                compileMultiWayBranchStatement mwbst

          | Z.Label_location_statement lablst =>
                compileLabelLocationStatement lablst

          | Z.Assert_statement asst =>
                compileAssertStatement asst
          | Z.Nop_statement =>
                compileNopStatement ()

    fun compileProcedure (sym, procTyp, SOME procBody) =
        let
            val _ = argNum := 0
            val {params = params, body = body} = procBody
            val _ = emitProcPrelude (sym)
            val frameOffset = compileLocalVars(params, false);
        in
            compileStatement body;
            emit "*"; newline ()
        end
      | compileProcedure (_, _, NONE) = raise MissingProcedureBody

    fun doFile fhdl = let
        val _ = procNum := 1
        val _ = initLabCount () 
        val _ = glo     := 0
        val {file_blocks = fileBlocks,
             type_table = {entries = typeEntries},
             symbol_table = {entries = symbolEntries},
             extern_symbol_table = {entries = externSymbolEntries},
             information_block = informationBlock} = Z.read_file_set_block fhdl

        val _ = buildTypeTab(typeEntries)
        val _ = buildSymbolTab(symbolEntries)
        val _ = buildExternSymbolTab(externSymbolEntries)

        fun doFileBlock {source_file_name = SourceFileName,
                         definition_block =
                         {defined_variables = definedVars,
                          defined_procedures = definedProcs}} =
            let
                fun doVariable sym = let
                    val {symbol = ste, refname = refname} = findSymbol sym
                    val varEntry = case ste of
                                       Z.VariableEntry x => x
                                     | _ => raise BadVariableEntry
                    val {key = key, address_taken = addressTaken,
                         def = {name = {uid = _, name = name}, type' = type',
                                value_block = valueBlock},
                         is_local = _} = varEntry
                in
                    compileVariable (sym, type', valueBlock)
                end

                fun doProcedure sym = let
                    val _ = initRegCount ()
                    val _ = initMulDivOps ()
		    val _ = initSEmited ()
                    val {symbol = ste, refname = _} = findSymbol sym
                    val procEntry = case ste of
                                        Z.ProcedureEntry x => x
                                      | _ => raise BadProcedureEntry
                    val {key = key, address_taken = addressTaken,
                         def = {name = _,
                                qualifications = qualifications,
                                procedure_type = procType,
                                procedure_body = procBody}} = procEntry
                in
                    compileProcedure (sym, procType, procBody);
                    procNum := !procNum + 1
                end
            in
                emit "Mbwrfd\n";
                app doVariable definedVars;
                app doProcedure definedProcs
            end
    in
        app doFileBlock fileBlocks;
        initTabs ()
    end

    fun main(_, argv) = let
        fun mkOutFile iFile =
            let
                val clist = L.rev (explode iFile)

                fun h ([], n) = (n, false)
                  | h (#"." :: _, n) = (n, true)
                  | h (_ :: r, n) = h (r, n + 1)

                val (index, found) = h (clist, 0)
                val name =
                    if found then
                        let
                            val suffix =
                                implode (L.rev (L.take (clist, index)))
                        in
                            if suffix = "zsuif" then
                                implode (L.rev (L.drop (clist, index + 1)))
                            else
                                raise (Fail "Bad magic number!")
                        end
                    else
                        raise (Fail "Bad magic number!")
            in
                name ^ ".dec"
            end

        fun analyze (file :: args) = (file, mkOutFile file) :: analyze (args)
          | analyze [] = []

        fun openOutFile (inf, outf) =
            (outFile := TextIO.openOut(outf); inf)

        val filePairs = analyze(argv)
    in
        app (fn x => (((withOpenFile doFile) o openOutFile) x;
                      TextIO.closeOut (!outFile))) filePairs;
        OS.Process.success
    end
end
