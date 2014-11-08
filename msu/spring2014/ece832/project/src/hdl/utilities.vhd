library IEEE;
use IEEE.math_real.all;

package utilities is
  pure function vector_length
    (X : in natural)
    return natural;
end;

package body utilities is

  pure function vector_length
    (X : in natural)
    return natural
  is
  begin
    if (x <= 1)
    then
      return natural(1);
    else
      return natural(ceil(log2(real(x))));
    end if;
  end function vector_length;

end package body;

