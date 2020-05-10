open Print

let news_scrot =
  {|----------------------------------
monday the 1st, 1982
EXM has entered the market at $50/share.
'E.T. the extra-terrestrial' becomes highest grossing film of all time
AT&T agrees to divest.
The falklands war is a thing.
Ronnie Regan still president.
----------------------------------
|}

let balance_scrot =
{|------------------------------------------
capital: -$7478, intrest rate: 0.010000
HCM: 0 ($0), JIM: 19 ($13015), PIE: 35 ($11970), EWA: 2 ($18020), IBY: 0 ($0),
portfolio value: $35527
margin remaining: $2522, intrest owed: $74.780000
-------------------------------------------
|}

let day_scrot =
{|------------------------------------------
trade today? [y/n]: y
markets are open!
9:00 $3322>

HCM: $174367 (-1%), JIM: $704 (-1%), PIE: $543 (+7%), EWA: $10106 (+5%), IBY: $0 (0%),
10:00 $3322>

HCM: $181482 (+4%), JIM: $737 (+4%), PIE: $565 (+4%), EWA: $10256 (+1%), IBY: $0 (0%),
11:00 $3322> buy 1 jim

HCM: $179668 (0%), JIM: $716 (-2%), PIE: $511 (-9%), EWA: $10056 (-1%), IBY: $0 (0%),
12:00 $2585> sell 2 pie

HCM: $179234 (0%), JIM: $721 (0%), PIE: $462 (-9%), EWA: $10047 (0%), IBY: $0 (0%),
1:00 $3607> s
----------------------------------------------
|}


let do_tutorial () =
  newln ();
  print_endline (dialogue "mr. peterson"
                   "righty-ho! this is your news screen. it will tell \
                   you pertinent financial news and alert you of \
                    anything that will affect you.");

  prompt_ret news_scrot;
  newln ();
  print_endline (dialogue "mr. peterson"
                   "and over here is your balance screen. it shows \
                    what stocks you own and how much money you have");
  prompt_ret balance_scrot;
  newln ();
  print_endline (dialogue "mr. peterson"
                   "if you aren't to hungover to show up to work, then \
                   you can trade during the 8 hours that markets are open");
  print_endline (dialogue "mr. peterson"
                   "every hour you can buy, sell, do nothing, or skip \
                    the rest of the day");
  prompt_ret day_scrot;
  newln ();
  print_endline (dialogue "mr. peterson"
                   "look. my horse needs a perm, so I have to go.");
  prompt_ret (dialogue "mr. peterson"
                "if you need anything ask peter.")


let do_intro () =
  clear_screen ();
  prompt_ret "the year is perpetually 1982.\nthe world has just \
              entered one of the greatest bull markets in the history \
              of ever.";
  prompt_ret "you have just graduated from the University of Alberta \
              school of business, one of the most prestigious \
              universities in alberta.";
  prompt_ret "your daddy pulled some strings to get you this job at \
              the bay street trading firm, peterson & sons.";
  newln ();
  print_endline (dialogue "mr. peterson" "hey new guy, whats your name?");
  Printf.printf "name: "; ignore (read_line ()); (* suckers *)
  newln ();

  let prompt = (dialogue "mr. peterson"
                  "never heard that one before. have you ever used \
                   one of these trade-o-tron-5000s before?")
  in
  if prompt_yn prompt
  then prompt_ret (dialogue "mr. peterson" "well then get back to work!")
  else do_tutorial ()
