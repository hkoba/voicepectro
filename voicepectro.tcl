#!/usr/bin/wish
# -*- mode: tcl; tab-width: 8 -*-

namespace eval vt {}
set vt::LICENSE "\
Copyright (C) 2006, 2013 hkoba <buribullet@gmail.com>

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.
"

package require snack
package require snit 1.0
package require BWidget
package require tile
package require Tktable

if {![llength [info commands ::console]]} {
    if {![catch {package require tclreadline}]} {
        puts "have readline"
        proc ::console method {
            switch $method {
                show {
		    if {[info exists ::tclreadline::_in_loop]} return
		    set ::tclreadline::_in_loop 1
		    after idle tclreadline::Loop
                }
                default {
                    error "Not yet supported: console $method"
                }
            }
        }
    }
}

namespace eval vt {
    proc widget {type name args} {
	if {[winfo exists $name]} {
	    eval [list $name configure] $args
	} else {
	    eval [list $type $name] $args
	}
	set name
    }
    proc set-widget {varName using type name args} {
	upvar 1 $varName $varName
	set $varName [eval [linsert $args 0 widget $type $name]]
    }
}

snit::widgetadaptor vt::app {
    delegate method * to hull
    delegate option * to hull
    #
    component sound -public sound
    component wf -public waveform
    component tab -public table
    component mesg
    component bf

    option -show-waveform 1

    foreach o {-width -height} {
	delegate option $o to wf
    }
    
    option -file
    onconfigure -file file {
	wm title [winfo toplevel $win] \
	    "[tk appname] -- loading $file ..."
	$self message "loading $file ..."
	$sound configure -file $file
	$self message ""
	wm title [winfo toplevel $win] \
	    "[tk appname] -- $file"
	update
	set fn [$self vmark-file]
	if {[file readable $fn]} {
	    # after idle では spectrogram が描画されない。
	    after idle [list after 500 [list $self load $fn]]
	}
    }
    oncget -file {
	$sound cget -file
    }

    typevariable numinst
    typeconstructor {
	set numinst 0
    }
    typemethod Open {fn args} {
	$type New -file $fn
    }
    typemethod New args {
	
	if {$numinst == 0} {
	    set top ""
	} else {
	    set top .vt$numinst
	    toplevel $top
	}
	incr numinst
	set obj $top.vt
	eval [list $type create $obj -width 800 -height 250] $args
	pack $obj -fill both -expand yes
	set obj
    }

    #
    constructor args {
	option add *Menu.tearOff 0

	installhull using ttk::paned -orient vertical

	# ボタン入れ場
	install bf using ttk::frame $win.bf	  

	# 波形
	install wf using vt::waveform $win.wf -table $win.tab

	# コメント一覧
	install tab using sntablesw $win.tab -cols 3 \
	    -model [$wf vmark-varname] -titlecols 2\
	    -colstretchmode last\
	    -usecommand yes -validate yes\
	    -header {start end comment}\
	    -altview $wf
	
	# メッセージ領域
	install mesg using message $win.mesg -text {}

	# 音声オブジェクト
	install sound using snack::sound $self.snd \
	    -changecommand $wf

	$self configurelist $args

	wm protocol [winfo toplevel $win] WM_DELETE_WINDOW \
	    [mymethod Close]

	# Menu
	[winfo toplevel $win] configure -menu [set m [menu $win.menu]]
	$m delete 0 end
	$m add cascade -label File -menu [menu $m.file]\
	    -underline 0
	$m.file add command -label Open -command [mymethod open]
	$m.file add command -label Save -command [mymethod save] \
	    -accelerator "Ctrl+S" -underline 0
	$m.file add separator
	$m.file add command -label Quit -command exit

	$m add cascade -label View -menu [menu $m.view]
	$m.view add checkbutton -label Waveform \
	    -variable [myvar options(-show-waveform)]\
	    -command [mymethod reconfigure]
	$m.view add separator
	$m.view add command -label "Spectrogram param..."\
	    -command [list vt::speg-control $win.sgc -top $wf]

	if {[info commands ::console] ne ""} {
	    $m add cascade -label Debug -menu [menu $m.debug]
	    $m.debug add command -label Console \
		-command [mymethod debug-console]
	}

	# 各ボタン
	set i 0
	pack [ttk::button $win.bf.b[incr i] -text Play -command \
		  [list $wf Play 0]] -side left
	pack [ttk::button $win.bf.b[incr i] -text Stop -command \
		  [list $sound stop]] -side left

	if {1} {
	    # 録音ボタン、作ってはみたものの、waveform item まわりが
	    # まるで動かなくなると判明。保存もだ。一旦お蔵入りに。
	    pack [ttk::separator $win.bf.b[incr i] -orient vertical] -side left
	    pack [ttk::button $win.bf.b[incr i] -text Record -command \
		  [mymethod Record]] -side left
	}
	

	# キーボード・ショートカット
	foreach w [list $win [$tab table] [$wf canvas]] {
	    bind $w <Control-s> [mymethod save]
	}

	# 
	after idle $win.wf configure -sound $sound

	# レイアウト
	$self reconfigure
    }

    method Close {} {
	if {[set w [winfo toplevel $win]] eq "."} {
	    destroy $win
	    wm withdraw $w
	} else {
	    destroy $w
	}
	set found 0
	foreach w [winfo children .] {
	    if {[winfo class $w] eq "Toplevel"} {
		incr found
	    }
	}
	if {! $found} exit
    }

    method reconfigure {} {
	# 画面から全部外して、
	foreach w [winfo children $win] {
	    if {[winfo manager $w] ne "paned"} continue
	    $self forget $w
	}

	# 足し直す
	$self add $bf -weight 0
	$self add $tab -weight 5
	if {$options(-show-waveform)} {
	    $self add $wf -weight 5
	}
	$self add $mesg -weight 0
    }

    method vmark-file {} {
	set fn [$self cget -file]
	if {$fn eq ""} {
	    error "No filename is specified"
	}
	return [file rootname $fn].vmark
    }
    method open {{fn ""}} {
	if {$fn eq ""} {
	    set types {}
	    lappend types [list MP3 .mp3]
	    lappend types [list WAV .wav]
	    set fn [tk_getOpenFile -title "Please choose sound file" \
			-filetypes $types]
	}
	if {$fn eq ""} {
	    return
	}
	if {[$self cget -file] ne ""} {
	    $type Open $fn
	} else {
	    $self configure -file $fn
	}
    }

    method load fn {
	puts loading-$fn
	set fh [open $fn]
	array unset _vmarks
	gets $fh header; # check してない。
	set ls {}
	while {[gets $fh line] >= 0} {
	    lappend ls [split $line \t]
	}
	close $fh
	update
	foreach desc [lsort -integer -index 0 $ls] {
	    eval [list $wf new-mark] $desc
	}

    }
    method save {{fn ""}} {
	if {$fn eq ""} {
	    set fn [$self vmark-file]
	}
	$tab flush
	$self busy
	set dump [$wf dump]
	set fh [open $fn.tmp w]
	puts $fh "#start\tend\tdesc"
	foreach i $dump {
	    puts $fh [join [lrange $i 1 end] \t]
	}
	close $fh
	file rename -force $fn.tmp $fn
	$self busy 0
	$self message "Saved!" 5000
    }

    method message {text {after ""}} {
	$mesg configure -text $text -width [winfo width $mesg]
	update
	if {$after ne ""} {
	    after $after [list $mesg configure -text {}]
	}
    }
}

snit::method vt::app busy {{onoff 1}} {
    if {$onoff} {
	set how watch
    } else {
	set how {}
    }
    foreach w [list $win [$wf canvas] $tab] {
	if {[catch {$w cget -cursor}]} continue
	$w configure -cursor $how
    }
    update idletask
}

snit::method vt::app debug-console {} {
    console show
    puts "vt::app is $self"
    puts "waveform is $wf"
}

snit::method vt::app Record {} {
    $wf itemconfigure -pixelspersecond 50
    $sound record
}

#========================================

snit::widgetadaptor ::sntablesw {
    component table; expose table
    delegate method * to table
    delegate option * to table

    delegate method setwidget to hull

    constructor args {
	installhull using ScrolledWindow
	install table using sntable $win.tab
	$win setwidget $table
	$self configurelist $args
    }
}

snit::widgetadaptor ::sntable {
    delegate method * to hull
    delegate option * to hull

    typeconstructor {
	# セル移動
	bind Table <Return>       {::tk::table::MoveCell %W  1  0}
	bind Table <Shift-Return> {::tk::table::MoveCell %W -1  0}
	bind Table <Tab>          {::tk::table::MoveCell %W  0  1; break}
	bind Table <Shift-Tab>    {::tk::table::MoveCell %W  0 -1; break}

	# 行内編集
	bind Table <Left> [bind Table <Control-Left>]
	bind Table <Right> [bind Table <Control-Right>]

	bind Table <Control-h> [bind Table <BackSpace>]

	bind Table <Control-b> [bind Table <Left>]
	bind Table <Control-f> [bind Table <Right>]
	bind Table <Control-n> [bind Table <Down>]
	bind Table <Control-p> [bind Table <Up>]

	# 数値キーパッドで Enter
	bind Table <KP_Enter> {event generate %W <Return>}	
    }

    variable _r2id -array {}

    option -altview {}

    option -header
    onconfigure -header value {
	set options(-header) $value
	$win set row 0,0 $value
    }

    option -model
    onconfigure -model varName {
	set options(-model) $varName
	trace add variable $varName write \
	    [mymethod update]
	trace add variable $varName unset \
	    [mymethod unset]

	upvar 1 $varName var
	set ls {}
	foreach i [array names var] {
	    lappend ls [list $i [lindex $var($i) 0]]
	}
	set r 0
	foreach desc [lsort -integer -index 1 $ls] {
	    # 結局呼ばれていない。
	    set id [lindex $desc 0]
	    set _r2id([incr r]) $id
	}
    }
    
    constructor args {
	installhull using table\
	    -anchor nw -selectmode extended -background white \
	    -titlerows 1 -titlecols 1 -ipadx 1 -cache 1\
	    -command [mymethod cellvalue %r %c]\
	    -browsecommand [mymethod commit %s]

	foreach dir {Up Down} {
	    bind $win <KeyRelease-$dir> [mymethod idletask $self recenter]
	}
	bind $win <ButtonRelease-1> [mymethod idletask $self recenter]

	$win tag configure title -background \#d7d3cf -foreground black\
	    -borderwidth 1 -relief raised\
	    -anchor n
	$win tag configure active -foreground black -background white

	$self configurelist $args
	$win activate origin
    }
    method row-item row {
	foreach i [$self tag names [$self item-tag *]] {
	    if {[$self tag row $i] == $row} {
		return [scan $i i%d]
	    }
	}
    }
    variable _idletask {}
    method idletask args {
	after cancel $_idletask
	set _idletask [after idle $args]
    }

    method caret-param index {
	# 全く制御しないと、IME カーソルが画面に全く出なくなってしまうので、
	# 少しでもましな場所に…
	set bbox [$self bbox $index]
	if {![llength $bbox]} return
	foreach {x y w h} $bbox break;
	# 一つ下のセルの真中。
	list -x [expr {$x + $w/2}] -y [expr {$y + $h}] -height $h
    }
    method set-caret {{index active}} {
	set caret [$self caret-param $index]
	if {![llength $caret]} return
	eval [list tk caret $win] $caret
    }

    method recenter {{index active}} {
	if {[set row [$self index $index row]] < 1} return
	$self set-caret
	set sample [$self get $row,0]
	if {$sample eq ""} return

	if {[set alt [$self cget -altview]] eq ""} return
	if {[winfo ismapped $alt]} {
	    $alt recenter $sample
	}
	$alt Play $sample
    }

    method cellvalue {r c} {
	if {$r < 1 || $c < 1} return
	if {![info exists _r2id($r)]} return
	if {![info exists options(-model)]} return
	upvar 0 $options(-model) src
	if {![info exists src($_r2id($r))]} return
	lindex $src($_r2id($r)) $c
    }
    
    method activate-id itemId {
	if {[set row [$self item-row $itemId]] eq ""} return
	$self activate $row,2
	$self see $row,2
    }

    method flush {} {
	set start [$self index active]
	$self activate origin
	$self activate end
	$self activate $start
    }

    method commit {{cell active}} {
	foreach {r c} [scan [$self index $cell] %d,%d] break
	if {$r < 1 || $c < 2} return
	set id [$self row-item $r]
	if {$id eq ""} {
	    return 0
	}
	if {![info exists options(-model)]} {
	    return 0
	}
	upvar 0 $options(-model) src
	if {![info exists src($id)]} {
	    puts "not editable? $id"
	    return 0
	}
	set val [$self get $cell]
	if {[lindex $src($id) 2] ne $val} {
	    if {[llength $src($id)] <= 2} {
		lappend src($id) $val
	    } else {
		lset src($id) 2 $val
	    }
	}
	return 1
    }
    method item-tag itemId {
	set tag i$itemId
    }
    method item-row itemId {
	$self tag row [$self item-tag $itemId]
    }

    method reset-r2id {} {
	array unset _r2id
	array set _r2id {}
	foreach tag [$self tag names i*] {
	    set id [scan $tag i%d]
	    set r [$self tag row $tag]
	    # puts $tag-$id-$r
	    if {[llength $r] > 1} {
		error "Invalid tag rows $r for $id"
	    }
	    set _r2id($r) $id
	}
    }

    method unset {arrayVar itemId op} {
	set tag [$self item-tag $itemId]
	if {![llength [set row [$self item-row $itemId]]]} return
	$self delete rows -holddimensions $row 1
	$self reset-r2id
    }
    method update {arrayVar itemId op} {
	upvar 1 $arrayVar model
	
	if {[set row [$self item-row $itemId]] eq ""} {
	    set row [expr {[array size _r2id] + 1}]
	    set _r2id($row) $itemId
	    if {[$self cget -rows] < $row + 1} {
		$self configure -rows [expr {$row + 1}]
	    }
	    $self tag row [$self item-tag $itemId] $row
	    # 新規の時だけ、activate
	    $self see $row,2
	    $self activate $row,2
	}
	$self set row $row,0 $model($itemId)
    }
}

#========================================

snit::widget vt::waveform {
    option -pixelspersecond 300
    option -colormap {
	#000 #006 #00B #00F #03F #07F #0BF #0FF #0FB #0F7 
	#0F0 #3F0 #7F0 #BF0 #FF0 #FB0 #F70 #F30 #F00
    }

    option -maxlen [expr {1024*512}]
    option -subsample 50

    option -mark-color blue
    option -play-color red
    option -drag-color blue
    option -drag-threshold 3

    option -table

    component sound -public sound
    delegate method * to sound
    # delegate option * to sound
    option -sound
    oncget -sound {set sound}
    onconfigure -sound value {
	set sound $value
	$self reconfigure
    }

    component canvas; expose canvas
    foreach opt {-width -height} {
	delegate option $opt to canvas
    }

    variable wf {}
    foreach meth {start end} {
	method $meth {} [string map [list %m $meth] \
			     {$canvas itemcget $wf -%m}]
    }
    method itemconfigure args {
	eval [list $canvas itemconfigure $wf] $args
    }
    component _sb
    component _bf

    #
    constructor args {
	$self reconfigure
	$self configurelist $args
	$self reconfigure
    }
    
    method New {} {$self reconfigure}
    method More {} {$self reconfigure}
    method Destroyed {} {$self reconfigure}
    method reconfigure {} {
	vt::set-widget _bf using frame $win.bf
	vt::set-widget canvas using canvas $win.canv\
	    -scrollregion {0 0 600 300}
	vt::set-widget _sb using scrollbar $win.sb \
	    -orient h -width 8 -command [mymethod yview]

	#========================================
	set i 0
	pack [vt::widget button $_bf.b[incr i] -text << \
		  -command [mymethod goto-mark -1]] -side left
	pack [vt::widget button $_bf.b[incr i] -text >> \
		  -command [mymethod goto-mark +1]] -side left

	bind $canvas <Enter> {focus %W}
	bind $canvas <Configure> [mymethod resize]
	bind $canvas <Delete> [mymethod Delete]

	if {$sound eq ""} return
	#========================================

	$canvas delete $wf
	array set opts [list -subsample [$self cget -subsample]]
	if {[$sound length] > [$self cget -maxlen]} {
	    set opts(-end) [$self cget -maxlen]
	} else {
	    set opts(-end) [$self length]
	}
	set wf [eval [list $canvas create waveform 0 0 -sound $sound] \
		    [array get opts]]

	#========================================
	$canvas bind $wf <ButtonPress-1> [mymethod press-1 %x %y]
	$canvas bind $wf <B1-Motion> [mymethod drag-1 %x %y]
	$canvas bind $wf <ButtonRelease-1> [mymethod release-1 %x %y]

	foreach tag {vmark vrange} {
	    $canvas bind $tag <1> [mymethod item-click-1]
	    $canvas bind $tag <3> [mymethod ItemMenu %X %Y]
	}

	#========================================
	pack $_bf -fill x -expand no
	pack $canvas -fill both -expand yes
	pack $_sb -fill x -expand no

	# scrollbar 幅の調整
	after idle $self scroll
	after idle $self resize
    }
    variable _dragStart {}
    variable _playStart {}

    method resize {} {
	$canvas itemconfigure $wf -width [$self width] -height [$self height]
	$self relocate-items [$self start] [$self end]
    }
    method width {} {
	winfo width $canvas
    }
    method height {} {
	winfo height $canvas
    }
    method clipped-samples {} {
	expr {[$self end] - [$self start]}
    }
    method x-to-sample x {
	expr {int(double($x)/[$canvas itemcget $wf -width]
		  * [$self clipped-samples]) + [$self start]}
    }
    method sample-to-x smp {
	expr {
	      ($smp - [$self start]) *
	      double([$canvas itemcget $wf -width]) / [$self clipped-samples]
	  }
    }
    method pps {} {
	$canvas itemcget $wf -pixelspersecond
    }
    method rate {} {
	$sound cget -rate
    }
    # array of {id start end atts...}
    variable _vmarks -array {}
    method dump {} {
	set list {}
	foreach {id desc} [array get _vmarks] {
	    if {[$canvas type $id] eq ""} {
		array unset _vmarks $id
		continue
	    }
	    lappend list [linsert $desc 0 $id]
	}
	lsort -integer -index 1 $list
    }

    variable _scrolltask {}
    variable _playmarktask {}
}

snit::method vt::waveform goto-mark dir {
    set dump [$self dump]
    if {[set id [$canvas find withtag goto]] eq ""} {
	set start [$self start]
	set end [$self end]
    } else {
	foreach {start - end -} [$canvas coords $id] break
	$canvas dtag $id goto
	foreach vn {start end} {
	    set $vn [$self x-to-sample [set $vn]]
	}
    }

    if {$dir < 0} {
	for {set i [expr {[llength $dump] -1}]} {$i >= 0} {incr i -1} {
	    if {[lindex $dump $i 2] + 1 < $end} break
	}
    } else {
	for {set i 0} {$i < [llength $dump]} {incr i} {
	    if {$start < [lindex $dump $i 1] - 1} break
	}	    
    }
    if {$i < 0 || $i >= [llength $dump]} return

    set id [lindex $dump $i 0]
    # puts i=($id)-$start-$end
    $canvas addtag goto withtag $id

    # スクロールし直し
    $self recenter [lindex $dump $i 1]

    if {[set tab [$self cget -table]] ne ""} {
	$tab activate-id $id
    }

    # 最後に再生
    $self PlayItem goto
}

snit::method vt::waveform recenter center {
    set opts [list -start [expr {$center - [$self clipped-samples]/2}]\
		  -end [expr {$center + [$self clipped-samples]/2}]]
    # puts id=$id,$opts
    eval [list $self scroll] $opts
}

snit::method vt::waveform Analyze {{tag current}} {
    set i [$canvas find withtag $tag]
    set wn $win.speg$i
    if {[winfo exists $wn]} {
	raise $wn
	wm deiconify $wn
	return
    }

    foreach {x1 - x2 -} [$canvas coords $i] break

    if {$x1 == $x2} return

    set opts [list -start [$self x-to-sample $x1] -end [$self x-to-sample $x2]]
    foreach o {-pixelspersecond -colormap} {
	lappend opts $o [$self cget $o]
    }
    eval [list vt::spectrogram $wn -sound $sound\
	     -height 200 -fftlength 512] $opts

    $wn set-waveform $self
}

snit::method vt::waveform ItemMenu {X Y} {
    set menu [vt::widget menu $win.imenu]
    $menu delete 0 end

    $menu add command -label Info \
	-command [mymethod ItemInfo [$canvas find withtag current]]
    $menu add separator
    $menu add command -label Delete \
	-command [mymethod Delete [$canvas find withtag current]]

    tk_popup $menu $X $Y 0
}

snit::method vt::waveform ItemInfo {{item current}} {
    foreach {start - end -} [$canvas coords $item] break
    puts [list -start [$self x-to-sample $start] -end [$self x-to-sample $end]]
}

snit::method vt::waveform Delete {{item current}} {
    if {[$canvas type $item] eq "waveform"} return
    set id [$canvas find withtag $item]
    if {$id ne ""} {
	array unset _vmarks $id
    }
    $canvas delete $item
}

snit::method vt::waveform item-click-1 {} {
    $self PlayItem current
    $self Analyze current
}


snit::method vt::waveform press-1 {x y} {
    focus $canvas
    if {[snack::audio active]} {
	$sound stop
	after cancel $_playmarktask
	catch {unset _dragStart}
    } else {
	set _dragStart [list $x $y]
	$self vmark $x $y press-start
    }
}

snit::method vt::waveform playmarktask msec {
    if {![snack::audio active]} {
	catch {unset _playmarktask}
	$canvas delete playmark
	return
    }
    set now [expr {int($_playStart
		       + [$self rate] * [snack::audio elapsedTime])}]
    set x [$self sample-to-x $now]
    if {$x > [$self width]} {
	$self scroll -start $now \
	    -end [expr {$now + [$self clipped-samples]}]
	set x 0
    }
    foreach {- top - bottom} [$canvas coords playmark] break
    $canvas coords playmark $x $top $x $bottom
    set _playmarktask [after $msec $self playmarktask $msec]
}

snit::method vt::waveform drag-1 {x y} {
    if {[llength [$canvas find withtag drag]]} {
	foreach {x1 top - bottom} [$canvas coords drag] break
	$canvas coords drag $x1 $top $x $bottom
    } elseif {[info exists _dragStart]} {
	foreach {oX oY} $_dragStart break
	set thr [$self cget -drag-threshold]
	if {abs($x - $oX) < $thr && abs($y - $oY) < $thr} return
	set opts [list \
		      [lindex $_dragStart 0] 0 \
		      $x [$self height]\
		      -fill [$self cget -drag-color] \
		      -activewidth 3\
		      -tags drag]

	lappend opts -stipple gray25
	eval [list $canvas create rectangle] $opts
    }
}

snit::method vt::waveform release-1 {x y} {
    if {![info exists _dragStart]} return
    set start [$self x-to-sample [lindex $_dragStart 0]]
    if {[llength [set id [$canvas find withtag drag]]]} {
	$canvas itemconfigure drag -tags vrange
	set del [$canvas find withtag press-start]
	unset _vmarks($del)
	$canvas delete $del
	set end [$self x-to-sample $x]
	set _vmarks($id) [list $start $end]
	$self PlayItem $id
    } else {
	$canvas itemconfigure press-start -tags vmark
	$self Play $start
    }
}

snit::method vt::waveform PlayItem {{item current}} {
    foreach {x1 - x2 -} [$canvas coords $item] break
    set opts [$self x-to-sample $x1]
    if {$x1 != $x2} {
	lappend opts [$self x-to-sample $x2]
    }
    eval [list $self Play] $opts
}

snit::method vt::waveform Play {start {end ""}} {
    set opts [list -start $start]
    if {$end ne ""} {
	lappend opts -end $end
    }
    set x [$self sample-to-x $start]
    set _playStart $start
    foreach {- top - bottom} [$canvas bbox $wf] break
    $canvas delete playmark
    $canvas create line $x $top $x $bottom -tags playmark \
	-fill [$self cget -play-color] -width 1
    $sound stop
    eval [list $sound play] $opts
    $self playmarktask 50
}


snit::method vt::waveform vmark-varname {} {
    myvar _vmarks
}

snit::method vt::waveform vmark {x y {tags vmark}} {
    set id [$canvas create line $x 0 $x [$self height]\
		-activewidth 3\
		-fill [$self cget -mark-color] -tags $tags]
    set _vmarks($id) [list [set pos [$self x-to-sample $x]] $pos]
}

snit::method vt::waveform new-mark {start end args} {
    set x [$self sample-to-x $start]
    if {$start == $end} {
	set id [$canvas create line $x 0 $x [$self height]\
		    -activewidth 3\
		    -fill [$self cget -mark-color] -tags vmark]
    } else {
	set x2 [$self sample-to-x $end]
	set opts [list -fill [$self cget -drag-color] \
		      -activewidth 3\
		      -tags vrange]
	lappend opts -stipple gray25
	set id [eval [list $canvas create rectangle \
			  $x 0 \
			  $x2 [$self height]]\
		    $opts]
    }
    set _vmarks($id) [linsert $args 0 $start $end]
}

snit::method vt::waveform relocate-items {start end} {
    # coords じゃ、item type 毎に動作が違う。困ったね。
    # これじゃ、line/rectangle 以外のもの(coords len が違う)を動かせない。
    foreach {id desc} [array get _vmarks] {
	foreach {st ed} $desc break
	set coords [$canvas coords $id]
	if {![llength $coords]} {
	    unset _vmarks($id)
	    continue
	}
	if {[llength $coords] != 4} {
	    puts "Unknown item coords $coords for id $id: [$canvas type $id]"
	    continue
	}
	set width [expr {-1 * ([lindex $coords 2] - [lindex $coords 0]) - 1}]
	set height [expr {[lindex $coords 3] - [lindex $coords 1]}]
	if {$ed < $start || $st > $end} {
	    lset coords 0 $width
	    lset coords 2 -1
	} else {
	    lset coords 0 [$self sample-to-x $st]
	    lset coords 2 [$self sample-to-x $ed]
	}
	$canvas coords $id $coords
    }
}

snit::method vt::waveform scroll args {
    if {![winfo exists $_sb]} return
    if {[llength $args]} {
	eval [list $self itemconfigure] $args

	array set opts $args
	$self relocate-items $opts(-start) $opts(-end)
    }
    eval [list $_sb set] [$self yview]
}

snit::method vt::waveform yview args {
    if {[llength $args] == 0} {
	if {[$self length] == 0} {
	    return [list 0 1]
	}
	lappend yview [expr {double([$self start]) / [$self length]}]
	lappend yview [expr {double([$self end]) / [$self length]}]
	# puts now,yview=$yview
	set yview
    } else {
	# puts args=$args
	if {[llength $args] == 1} {
	    set args [linsert $args 0 moveto]
	}
	switch -exact -- [lindex $args 0] {
	    moveto {
		set start [expr {int([lindex $args 1] * [$self length])}]
		set end [expr {[$self clipped-samples] + $start}]
	    }
	    scroll {
		foreach {num unit} [lrange $args 1 end] break
		# nop
		set diff [switch $unit {
		    units {
			expr {$num * [$self clipped-samples]}
		    }
		    pages {
			expr {$num * [$self clipped-samples]}
		    }
		}]
		set start [expr {[$self start] + $diff}]
		set end [expr {[$self end] + $diff}]
	    }
	}
	if {$start < 0} {
	    set start 0
	    set end [$self clipped-samples]
	} elseif {$end > [$self length]} {
	    set end [$self length]
	    set start [expr {$end - [$self clipped-samples]}]
	}
	# puts start=$start,end=$end
	after cancel $_scrolltask
	set _scrolltask [after idle \
			     $self scroll -start $start -end $end]
    }
}

#----------------------------------------

snit::widget vt::spectrogram {
    component canvas; expose canvas

    foreach opt {
	-anchor
	-tags
	-sound
	-height
	-width
	-fftlength
	-winlength
	-preemphasisfactor
	-pixelspersecond
	-start
	-end
	-brightness
	-contrast
	-topfrequency
	-gridtspacing
	-gridfspacing
	-channel
	-colormap
	-progress
	-gridcolor
	-windowtype
    } {
	option $opt -cgetmethod itemcget -configuremethod itemconfigure
    }

    variable speg

    #========================================

    hulltype toplevel
    constructor args {
	[ScrolledWindow $win.sw] setwidget\
	    [vt::set-widget canvas using canvas $win.sw.canv]

	bind $canvas <Configure> [mymethod resize]
	bind $canvas <Enter> {focus %W}
	bind $canvas <Delete> [mymethod delete-item]

	# 初期化途中で単発で configure がかかると、死ぬ。
	# なので、最初は全部の引数を一度に create に渡す
	set speg [eval [list $canvas create spectrogram 0 0] $args]
	$canvas configure -width [$canvas itemcget $speg -width]
	$canvas configure -height [$canvas itemcget $speg -height]

	$canvas bind $speg <ButtonPress-1> [mymethod press-1 %x %y]
	$canvas bind $speg <B1-Motion> [mymethod drag-1 %x %y]
	$canvas bind $speg <ButtonRelease-1> [mymethod release-1 %x %y]

	pack $win.sw -fill both -expand yes
    }
    method itemconfigure args {
	eval [list $canvas itemconfigure $speg] $args
    }
    method itemcget name {
	$canvas itemcget $speg $name
    }

    method delete-item {} {
	foreach id [$canvas find withtag current] {
	    if {[$canvas type $id] eq "rectangle"} {
		$canvas delete $id
	    }
	}
    }

    variable _dragStart {}
    method press-1 {x y} {
	set _dragStart [list $x $y]
    }
    method drag-1 {x y} {
	set color palegreen
	set thr 3

	foreach {oX oY} $_dragStart break
	if {abs($x - $oX) < $thr && abs($y - $oY) < $thr} {
	    return
	} elseif {[llength [$canvas find withtag drag]]} {
	    foreach {x1 top - bottom} [$canvas coords drag] break
	    $canvas coords drag $x1 $top $x $bottom
	} else {
	    set opts [list \
			  [lindex $_dragStart 0] 0 \
			  $x [$self itemcget -height]\
			  -fill $color \
			  -activewidth 3\
			  -tags drag]
	    lappend opts -stipple gray25
	    eval [list $canvas create rectangle] $opts
	}
    }
    method release-1 {x y} {
	if {[llength [set id [$canvas find withtag drag]]]} {
	    $canvas dtag $id drag
	    $canvas addtag subsect withtag $id
	    $canvas bind $id <1> [mymethod PlayItem $id]
	    $canvas bind $id <Delete> [list $canvas delete $id]
	} else {
	    $self Play
	}
    }

    # $wf を configure で渡せないので、専用の関数を用意する。
    variable wf
    method set-waveform w {
	set wf $w
    }
    method Play {} {
	$wf Play [$self cget -start] [$self cget -end]
    }
    method PlayItem id {
	foreach {start - end -} [$canvas coords $id] break
	$wf Play [$self x-to-sample $start] [$self x-to-sample $end]
    }
    method resize {} {
	# puts [winfo geometry $canvas]
	set bw [$canvas cget -borderwidth]
	set opts {}
	foreach o {width height} {
	    lappend opts -$o [winfo $o $canvas]
	}
	eval [list $canvas itemconfigure $speg] $opts
    }

    method clipped-samples {} {
	expr {[$self itemcget -end] - [$self itemcget -start]}
    }
    method x-to-sample x {
	expr {int(double($x)/[$self itemcget -width]
		  * [$self clipped-samples]) + [$self itemcget -start]}
    }
}

#----------------------------------------

snit::widget vt::speg-control {
    hulltype toplevel

    option -top
    variable speglist {}
    constructor args {

	wm title [winfo toplevel $win] "Spectrogram Controler"

	set i 0

	foreach {title opts} {
	    Brightness {
		-from -100 -to 100 -res 0.1
	    }

	    Contrast {
		-from -100 -to 100 -res 0.1
	    }
	} {
	    lappend wl [eval [list scale $win.s[incr i] -label $title \
				  -orient hori -length 100 \
				  -command [mymethod itemconf \
						-[string tolower $title]]
				 ] $opts]
	}
	$self configurelist $args

	bind $win <Enter> [mymethod rescan]
	eval pack $wl -side top
    }
    method rescan {} {
	set speglist {}
	foreach w [winfo children [$self cget -top]] {
	    if {[winfo class $w] ne "Spectrogram"} continue
	    lappend speglist $w
	}
	set speglist
    }
    method itemconf {name value} {
	foreach w $speglist {
	    $w itemconfigure $name $value
	}
    }
    method speglist {} {
	set speglist
    }
}

#----------------------------------------

if {[info exists ::argv0] && [info script] == $::argv0
    && ![winfo exists .vt]
} {
    if {[llength $argv]} {
	vt::app Open [lindex $argv 0]
    } else {
	vt::app New
    }
}
