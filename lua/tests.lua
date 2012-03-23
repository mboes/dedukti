require "dedukti";

function printc(n, c)
  assert(c.ck);
  local v = mkcode(cvar, n);
  if c.ck == clam then
    io.write("(Lam "..n..". ");
    printc(n+1, c.clam(v));
    io.write(")");
  elseif c.ck == cpi then
    io.write("(Pi "..n.." : ");
    printc(n, c.cpi[1]);
    io.write(". ");
    printc(n+1, c.cpi[2](v));
    io.write(")");
  elseif c.ck == capp then
    io.write("(");
    printc(c.capp[1]);
    io.write(" ");
    printc(c.capp[2]);
    io.write(")");
  elseif c.ck == ctype then
    io.write("Type");
  elseif c.ck == ckind then
    io.write("Kind");
  elseif c.ck == cvar then
    io.write("Var "..c.cvar);
  elseif c.ck == ccon then
    io.write("Con "..c.ccon);
  else
    error("Unknown code type.");
  end
end

function mkarr(dom, codom)
  return { ck = cpi; cpi = { dom, function (x) return codom; end } };
end

local aty = { ck = cvar; cvar = 'a' };
local bty = { ck = cvar; cvar = 'b' };

local idt = { tk = tlam, tlam = function (x_box) return x_box; end };
local idty = mkarr(aty, aty);
print("Checking identity: "..tostring(check(0, idt, idty)));

local idty = mkarr(aty, bty);
print("Checking identity (wrong type): "..tostring(check(0, idt, idty)));

local two =
  { tk = tlam
  , tlam = function (f_box)
      local f = obj(f_box);
      return
        { tk = tlam
        , tlam = function (x_box)
            local x = obj(x_box);
            return
              { tk = tapp
              , tapp =
                { f_box
                , { tk = tubox
                  , tubox = { { tk = tapp, tapp = { f_box, x_box } }
                            , { ck = capp, capp = ap(f, x) }
                            }
                  }
                }
              };
          end
        };
    end
  };
local twoty = mkarr(mkarr(aty, aty), mkarr(aty, aty));
printc(0, twoty);
io.write("\n");
print("Checking Church's two: "..tostring(check(0, two, twoty)));

-- vi: expandtab
