{
  # Home row mods, but only on the laptop's internal keyboard: keyd matches
  # devices by vendor:product id, and the built-in i8042 keyboard always shows
  # up as "AT Translated Set 2 keyboard" (0001:0001). External USB/BT keyboards
  # don't match, so they keep stock behavior.
  #
  # Idiom from keyd(1) for home row mods:
  #   overloadi(key, overloadt2(mod, key, hold), idle)
  # - overloadi: if another letter was struck less than <idle> ms ago we're in
  #   a typing flow, so resolve as the plain letter (no mod attempt at all —
  #   this is what prevents mistypes during fast typing/rolls);
  # - overloadt2: otherwise tap = letter, hold >= <hold> ms = modifier.
  configurations.nixos.pike.module =
    let
      homeRowMod = mod: key: "overloadi(${key}, overloadt2(${mod}, ${key}, 200), 200)";
    in
    {
      services.keyd = {
        enable = true;
        keyboards.internal = {
          ids = [ "0001:0001" ];
          settings.main = {
            # left hand: a=super, s=alt, d=ctrl, f=shift
            a = homeRowMod "meta" "a";
            s = homeRowMod "alt" "s";
            d = homeRowMod "control" "d";
            f = homeRowMod "shift" "f";
            # right hand, mirrored
            j = homeRowMod "shift" "j";
            k = homeRowMod "control" "k";
            l = homeRowMod "alt" "l";
            ";" = homeRowMod "meta" ";";
          };
        };
      };
    };
}
