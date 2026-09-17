{
  # Home row mods, but only on the laptop's internal keyboard: keyd matches
  # devices by vendor:product id, and the built-in i8042 keyboard always shows
  # up as "AT Translated Set 2 keyboard" (0001:0001). External USB/BT keyboards
  # don't match, so they keep stock behavior.
  #
  # Idiom from keyd(1) for home row mods:
  #   lettermod(mod, key, idle, hold)
  #     == overloadi(key, overloadt2(mod, key, hold), idle)
  # - idle: if another symbol key was struck less than <idle> ms ago we're in a
  #   typing flow, so the key resolves as the plain letter and no modifier is
  #   possible at all (this is what prevents mistypes during fast rolls).
  #   Keep it below the inter-key interval of normal typing (~80-150 ms is
  #   fast); at 200 ms every keystroke blocks the mods for a fifth of a second,
  #   which forces a deliberate pause before every chord.
  # - hold: outside the idle window, tap = letter, hold >= <hold> ms = modifier;
  #   overloadt2 also resolves as a hold when another key is tapped meanwhile.
  configurations.nixos.pike.module =
    let
      homeRowMod = mod: key: "lettermod(${mod}, ${key}, 100, 180)";
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
