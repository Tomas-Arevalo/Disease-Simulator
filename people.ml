
module G = Graphics ;;
open Config ;;
open Registry ;;
module Ctr = Counter ;;
module Viz = Visualization ;;
module Stat = Statistics ;; 
(* module Utilities = Utilities ;; *) 

(*....................................................................
                                People
 *)
  
class person (initx : int) (inity : int)
             (initstepsize : int)
             (initinfect : float) =
  object (self)
    val id : string = Utilities.gensym ()
    val mutable posx : int = initx
    val mutable posy : int = inity
    val mutable step_size : int = initstepsize
    val mutable infectiousness : float = initinfect

    (* Grocery Time; aactive: true if at store false if not *)
    val mutable store_time = 5
    val mutable active = false

    (* Used for position before grocery/before central quarantine *)
    val mutable ogposx = initx
    val mutable ogposy = inity
                  
    method id : string = id
    method step_size : int = step_size
    method infectiousness : float = infectiousness
                  
    method set_pos (x : int) (y : int) : unit =
      posx <- x;
      posy <- y
    method pos = posx, posy
                         
    method set_step_size (new_step_size : int) : unit =
      step_size <- new_step_size
                     
    method set_infectiousness (new_infect : float) : unit =
      infectiousness <- new_infect

    method move : unit =
      let x, y = self#pos in
      let newx, newy =
        Utilities.rand_step x y self#step_size in
      (* drop from old location in registry *)
      Reg.deregister (self :> thing_type);
      (* update location *)
      self#set_pos newx newy;
      (* re-add at the new location *)
      Reg.register (self :> thing_type)

    method update : unit =
      self#move;

      (* Grocery Stores Extension *)
      (* let random_pair () =
        let stores = [(50,50); (50,150); (150,50); (150,150)] in
        let index = Random.int (List.length stores) in
        List.nth stores index
      in
      let store_prob = 0.2 in
      let store = random_pair () in
      if store_time = 5 && Utilities.flip_coin store_prob then
        begin
          active <- true;
          self#set_pos (fst store) (snd store);
          ogposx <- fst self#pos;
          ogposy <- snd self#pos
        end
      else ();
      if active then 
        begin
          if store_time < 5 && store_time <> 0 then 
              store_time <- store_time - 1
          else 
            begin
              self#set_pos ogposx ogposy;
              store_time <- 5;
              active <- false
            end
        end *)
  
    method draw : unit =
      let x, y = self#pos in
      Viz.draw_circle x y G.black
  end ;;

(*....................................................................
                       People in various states

  Note that since these classes refer to each other, they must be
  simultaneously defined using `and` instead of sequentially defined
  as separate classes.
 *)
  
class susceptible (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
                   cSTEP_SIZE_SUSCEPTIBLE
                   cINFECTIOUSNESS_SUSCEPTIBLE
            as super

    initializer
      Stat.susceptible#bump
                     
    method! update =
      super#update;
      let posx, posy = self#pos in
      (* calculate total infectiousness of all neighbors *)
      let infectiousness_total =
        Utilities.sum_float
          (List.map (fun obj -> obj#infectiousness)
                    (Reg.neighbors (self :> thing_type))) in
      (* if infected, update the registry by replacing this object
         with an infected one *)
      if Utilities.flip_coin infectiousness_total then
        begin
          Stat.susceptible#debump;
          Reg.deregister (self :> thing_type);
          Reg.register ((new infected posx posy) :> thing_type)
        end

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_SUSCEPTIBLE
  end

and (* class *) infected (initx : int) (inity : int) =
  object(self)
    inherit person initx inity
                   cSTEP_SIZE_INFECTED
                   cINFECTIOUSNESS_INFECTED
                   as super

    initializer
      Stat.infected#bump

    

    (*.................................................................
      Place any augmentations to `infected` here.
    ................................................................ *)
    val mutable rec_time = 
      let mean, stdev = cRECOVERY_PERIOD in
      int_of_float (Utilities.gaussian mean stdev)

    (* Central Quarantine Variables *)
    (* count for how much time has passed since first got infected;
    quarantine is how many time steps passed until they begin quarantine*)
    val mutable count = 0
    val quarantine = 
      let mean, stdev = cRECOVERY_PERIOD in
      (int_of_float (Utilities.gaussian mean stdev))

    (* Max Capacity Variable *)
    val cap = 10000
    
    method! update : unit =
      super#update;
      let posx, posy = self#pos in
      if rec_time = 0 then
        (* Max Capacity Extension *)
        let death_rate =
          if Stat.infected#count > cap then
            cMORTALITY +. 0.2
          else
            cMORTALITY
        in
        if Utilities.flip_coin death_rate then
          begin
            Stat.infected#debump;
            Reg.deregister (self :> thing_type);
            Reg.register ((new deceased posx posy) :> thing_type)
          end
        else
          begin
            Stat.infected#debump;
            Reg.deregister (self :> thing_type);
            (* Returns to position before central quarantine *)
            (* Reg.register ((new recovered ogposx ogposy) :> thing_type) *)
            Reg.register ((new recovered posx posy) :> thing_type)
          end
      else
        rec_time <- rec_time - 1;

        (* Central Quarantining Extension *)
        (* count <- count + 1;
        if count = (10) && Utilities.flip_coin 0.5 then
          begin
            ogposx <- fst self#pos;
            ogposy <- snd self#pos;
            self#set_pos 100 100;
            self#set_infectiousness 0. ;
            self#set_step_size 0
          end *)

    method! draw : unit =
      let x, y = self#pos in
      Viz.draw_circle ~size:8 ~filled:false x y cCOLOR_INFECTED;
      Viz.draw_circle x y cCOLOR_INFECTED

  end

(*....................................................................
Place definitions for any other classes here. In particular, you'll
want to at least implement a `recovered` class for `infected` people
who have recovered from the infection and a `deceased` class for
`infected` people who do not recover.
....................................................................*)
and (* class *) recovered (initx : int) (inity : int) =
  object(self)
    inherit person initx inity
                  cSTEP_SIZE_RECOVERED
                  cINFECTIOUSNESS_RECOVERED
                  as super

    initializer
      Stat.recovered#bump

    val mutable immunity = 
      let mean, stdev = cIMMUNITY_PERIOD in
      int_of_float (Utilities.gaussian mean stdev)

    method! update = 
      super#update;
      if immunity = 0 then
        begin
          Stat.recovered#debump;
          Reg.deregister (self :> thing_type);
          Reg.register ((new susceptible posx posy) :> thing_type)
        end
      else 
        immunity <- immunity - 1

    method! draw : unit =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_RECOVERED

    end

and (* class *) deceased (initx : int) (inity : int) =
  object(self)
    inherit person initx inity
                  cSTEP_SIZE_DECEASED
                  cINFECTIOUSNESS_DECEASED

    initializer
      Stat.deceased#bump

    method! draw : unit =
      let x, y = self#pos in
      Viz.draw_cross x y cCOLOR_DECEASED

  end

;;
