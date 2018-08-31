
structure Test = 
    struct

        structure G = GetOpt

        datatype flag = 
            Verbose 
          | Version 
          | Name of string 
          | Output of string 
          | Arg of string 

        fun show (Verbose) = "Verbose"
          | show (Version) = "Version"
          | show (Name s) = concat ["Name \"",s,"\""]
          | show (Output s) = concat ["Output \"",s,"\""]
          | show (Arg s) = concat ["Arg \"",s,"\""]

        fun out NONE = Output "stdout"
          | out (SOME o') = Output o'

        val options = [{short="v",
                        long=["verbose"],
                        desc=(G.NoArg Verbose),
                        help="verbosely list files"},
                       {short="V?",
                        long=["version","release"],
                        desc=(G.NoArg Version),
                        help="show version info"},
                       {short="o",
                        long=["output"],
                        desc=(G.OptArg (out,"FILE")),
                        help="use FILE for dump"},
                       {short="n",
                        long=["name"],
                        desc=(G.ReqArg (Name,"USER")),
                        help="only USER's files"}]
            
        fun listify (disp,l) = 
            let fun listify' (disp,[]) = "]"
                  | listify' (disp,[x]) = (disp x)^"]"
                  | listify' (disp,x::xs) = concat [disp x,",",listify' (disp,xs)]
            in
                "["^(listify' (disp,l))
            end

        val header = "Usage: foobar [OPTION...] files..."
            
        fun test' order cmdline = 
            let 
                val (o',n) = G.getOpt order options cmdline
            in
                concat ["options=",listify (show,o'),
                        "  args=",listify (fn x=>concat["\"",x,"\""],n),
                        "\n"]
            end
        
        fun test order cmdline = print (test' order cmdline)
                                    handle G.Error (s) => (print (concat [s,G.usageInfo header options]))


    end

(* Here is a sample run with the above code:
   (after loading "getopt-sig.sml", "getopt.sml", and "test.sml")

- Test.test GetOpt.RequireOrder ["foo","-v"];
options=[]  args=["foo","-v"]
val it = () : unit
- Test.test GetOpt.Permute ["foo","-v"];
options=[Verbose]  args=["foo"]
val it = () : unit
- Test.test (GetOpt.ReturnInOrder Test.Arg) ["foo","-v"];
options=[Arg "foo",Verbose]  args=[]
val it = () : unit
- Test.test GetOpt.Permute ["foo","--","-v"];
options=[]  args=["foo","-v"]
val it = () : unit
- Test.test GetOpt.Permute ["-?o","--name","bar","--na=baz"];
options=[Version,Output "stdout",Name "bar",Name "baz"]  args=[]
val it = () : unit
- Test.test GetOpt.Permute ["--ver","foo"];
option `--ver' is ambiguous; could be one of:
  -v      --verbose             verbosely list files
  -V, -?  --version, --release  show version info   
Usage: foobar [OPTION...] files...
  -v        --verbose             verbosely list files
  -V, -?    --version, --release  show version info   
  -o[FILE]  --output[=FILE]       use FILE for dump   
  -n USER   --name=USER           only USER's files   
val it = () : unit

*)
