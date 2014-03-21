using System;
using System.Collections.Generic;

namespace AkomachiCS
{
    public class AkomachiCS
    {
        AkomachiCS()
        {
            Akomachi.Akomachi ako = new Akomachi.Akomachi();
            Akomachi.Parser.Result result = ako.parse("global.z = fun(x,y,z) {x+y+z}; z(1,2,3);");
            if( result.IsSuccess ) {
                Akomachi.Parser.Result.Success succ = (Akomachi.Parser.Result.Success)result;
                Akomachi.Runtime.Value v = ako.dance(succ.Item);
                if(v.IsInt){
                    Akomachi.Runtime.Value.Int b = (Akomachi.Runtime.Value.Int)v;
                    int i1 = b.Item;
                }
                // or
                int i2 = Akomachi.Runtime.value2int(v);
            }
        }
    }
}
