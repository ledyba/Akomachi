using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace AkomachiRunCS
{
    class Sys
    {
        public void println(String src)
        {
            Console.WriteLine(src);
        }
        public void print(String src)
        {
            Console.Write(src);
        }
    }
    class Program
    {
        static void Main(string[] args)
        {
            Akomachi.Akomachi ako = new Akomachi.Akomachi();
            ako.setGlobalObject("System", (Object)new Sys());
            if(args.Length > 0) {
                System.IO.StreamReader stream = System.IO.File.OpenText (args[0]);
                String src = stream.ReadToEnd();
                Akomachi.Parser.Result parseResult = ako.parse(src);
                if( parseResult.IsSuccess ) {
                    Akomachi.Parser.Result.Success ast = (Akomachi.Parser.Result.Success)parseResult;
                    Akomachi.Runtime.Value ret = ako.dance(ast.Item);
                }else{
                    Akomachi.Parser.Result.Error err = (Akomachi.Parser.Result.Error)parseResult;
                    Console.WriteLine("Failed to parse: {0}", err.Item);
                }
            }else{
                Console.WriteLine("Usage: {0} <src>" , System.AppDomain.CurrentDomain.FriendlyName);
            }
        }
    }
}
