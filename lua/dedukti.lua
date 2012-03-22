-- Dedukti LUA basic runtime.

-- Terms can be of 6 kinds, either a lambda, a product, an
-- application, type, a user created box, or a box.
--
-- In a term object, two fields must be present, 'tk',
-- indicating the kind of the term, and a field with
-- the name of the kind.

-- Code can be of 7 kinds, either a lambda, a product, an
-- application, type, kind, a variable, a constant.

tlam, tpi, tapp, ttype, tubox, tbox =       -- Possible tk
  'tlam', 'tpi', 'tapp', 'ttype', 'tubox', 'tbox';

clam, cpi, capp, ctype, ckind, cvar, ccon = -- Possible ck
  'clam', 'cpi', 'capp', 'ctype', 'ckind', 'cvar', 'ccon';

function mkcode(kind, ob) -- Building code.
  assert(kind == clam or kind == cpi or kind == capp
      or kind == ctype or kind == ckind or kind == cvar
      or kind == ccon); -- I miss type safety.
  local c = { ck = kind };
  c[kind] = ob;
  return c;
end

function ap(a, b)
  assert(a.ck and b.ck);
  if a.ck == clam then
    return a.clam(b);
  else
    return mkcode(capp, { a, b });
  end
end

function obj(t)
  assert(t.tk && t.tk == tbox);
  return t.tbox[2];
end

function convertible(a, b)
  local function conv(n, a, b)
    assert(a.ck and b.ck);
    local v = mkcode(cvar, n);
    if a.ck == cvar and b.ck == cvar then
      return a.cvar == b.cvar;
    elseif a.ck == ccon and b.ck == ccon then
      return a.ccon == b.ccon;
    elseif a.ck == clam and b.ck == clam then
      return conv(n+1, a.clam(v), b.clam(v));
    elseif a.ck == cpi and b.ck == cpi then
      return conv(n, a.cpi[1], b.cpi[1])
         and conv(n+1, a.cpi[2](v), b.cpi[2](v));
    elseif a.ck == capp and b.ck == capp then
      return conv(n, a.cpi[1], b.cpi[1])
         and conv(n, a.cpi[2], b.cpi[2]);
    elseif a.ck == ctype and b.ck == ctype then
      return true;
    elseif a.ck == ckind and b.ck == ckind then
      return true;
    else
      return false;
    end
  end
  return conv(0, a, b);
end

function box(cty, cv) -- Creates a Term box.
  return { tk = tbox, tbox = { cty, cv } };
end

--[[ Typechecking functions. ]]

function synth(n, t)
  assert(t.tk);
  if t.tk == tbox then
    return t.tbox[1];
  elseif t.tk == ttype then
    return { ck = ckind };
  elseif t.tk == tapp and t.tapp[2].tk == tbox then
    local b = t.tapp[2].tbox;
    local c = synth(n, t.tapp[1]);
    assert(c.ck == cpi and convertible(c.cpi[1], b[1])); -- This is totally ugly.
    return c.cpi[2](b[2]);
  elseif t.tk == tapp and t.tapp[2].tk == tubox then
    local b = t.tapp[2].tubox;
    local ty = synth(n, b[1]);
    local c = synth(n, t.tapp[1]);
    assert(c.ck == cpi and convertible(c.cpi[1], ty));
    return c.cpi[2](b[2]);
  elseif t.tk == tpi and t.tpi[1].tk == tbox
     and t.tpi[1].tbox[1].tk == ttype then
    return 42;
  end
end

function check(n, t, c) -- t is a term, c is a type.
  assert(t.tk and c.ck);
  local v = mkcode(cvar, n);
  if t.tk == tlam and c.ck == cpi then
    return check(n+1, t.tlam(box(c.cpi[1], v)), c.cpi[2](v))
  else
    return convertible(synth(n, t), c);
  end
end

return 42;
-- vi: expandtab
