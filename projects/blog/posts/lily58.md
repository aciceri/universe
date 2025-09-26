---
title: My first assembled mechanical keyboard
tags: [mechanical, keyboard]
date: 2021-04-22
language: en
---

To be honest I built this keyboard the last summer, I took pictures with the
idea of documenting the process in a post but I never felt like it, until now,
and now I obviously forgot a lot of details I wanted to write down.

The keyboard is a [Lily58](https://github.com/kata0510/Lily58), a "6*4+4keys column-staggered split keyboard", I only
found the normal version, not "Pro". Since a split keyboard wasn't enough I've
also decided to use blank caps.
The reason behind this choice was that I was not sure about the layout when I
ordered the parts, and since it was a long time that I wanted to learn to blind
type I thought it would have been convenient.
I have to partially change my mind, I spent some days using sites like [keybr](https://www.keybr.com/) to
improve my skills but as soon I need more unusual symbols (not the characters in
short) I've difficulties.

![The keyboard on my desk](/images/lily58/result3.jpg)
*The keyboard on my desk*

Then, when in September I needed to type efficiently and I abondoned this
keyboard.
In conclusion, in hindsight, it would have been better to choose a less exotic
keyboard, I would had more satisfaction.

## Parts

I found out that trying to get the parts as European citzen is not easy, there's
little choice an the prices are high. I didn't find any Italian resellers for
this kind of stuff.

However in the end I ordered the needed parts from
- [MechBoards](https://mechboards.co.uk/) for the PCB, the diodes, the buttons, the TRRS cable (and
  connectors) and the acrylic case.
- [this store](https://www.aliexpress.com/store/429151) on AliExpress for the switches, the keycaps and the Pro Micros.

Usually I don't like to advertise for free, but this time I'm happy to do it
because the both the stores were really kind. The first one resent me a piece I
accidentaly broke and the second one let me personalize my order with the two
central 1.5U grey keys.

The total cost was about 120 euros, shipping included.
However I've some leftovers, some caps and switches, I could build a useless
numpad.

### Case, cable, buttons and connectors

The case is in acrylic and it's transparent, had a protection film that made me
really satisfied when I removed it.

![Acrylic case from MechBoard](/images/lily58/case.jpg)
*Acrylic case from MechBoard*

With the case I also got the TRSS cable and two females connectors.

![TRSS cable, female connectors and buttons](/images/lily58/jack-and-buttons.jpg)
*TRSS cable, female connectors and buttons*

Obviously with the case there were also the needed screws and some rubber pads,
however in the end I used other pads I already had because they were higher.

![Screws](/images/lily58/screws.jpg)
*Screws*

### The PCBs and the diodes

The two PCBs are identicals and they work differently depending on the side you
sold components on, an ingenious and cost-effective design.

![Lily58, black is cool](/images/lily58/pcb1.jpg)
*Lily58, black is cool*

![The other side](/images/lily58/pcb2.jpg)
*The other side*

### A lot of diodes

![Diodes](/images/lily58/diodes.jpg)
*Diodes*

### Microcontrollers

Two Arduino Pro Micro are needed, I got them on AliExpress because they were
cheaper. One of them gave me problems, after I soldered it I realized the
flashing didn't work.

![The Pro Micro pinout](/images/lily58/promicro.jpg)
*The Pro Micro pinout*

I'm a noob and this was a good lesson, next time I'll flash both the
microcontrollers before soldering them, however I don't know if the Pro Micro
was already broken or I damaged it during the soldering.

### Display

The Lily58 supports two SSD1306 displays, since ~~I broke one of them~~ one of
them is useless because it only shows a static logo I used only one.

![The SSD1306](/images/lily58/lcd.jpg)
*The SSD1306*

Considering the negligible cost (about 1 dollar from AliExpress) I think I'll
reuse this display in future projects.

### Switches and caps

For this keyboard it's important to pay attention to profile of the keycaps
because of the exotic layout.
My choice fell on blank keycaps with the XDA profile (all the caps are the same)
so this wasn't a problem.

![The SSD1306](/images/lily58/caps-and-switches.jpg)
*The SSD1306*

For the switches I chose the blue MX Gateron, I alredy tried them and I really
liked the clicky feedback.
Moreover they were quite cheap on AliExpress.

### A sleepy watchdog

My trusty watchdog oversaw and certified the entire procedure.

![A good boy](/images/lily58/marley.jpg)
*A good boy*

## Assembling

I followed [this guide](https://kata0510.github.io/Lily58-Document/Lily58_BG.html), even if I don't speak japanese I was able to understand
thanks to the numerous images.

![The schematic](/images/lily58/schematic.png)
*The schematic*

The soldering was quite easy, I thought it would be more difficult, maybe the
merit is of the PCB of excellent build quality.

![Soldering the diodes](/images/lily58/soldering1.jpg)
*Soldering the diodes*

![The matrix of diodes](/images/lily58/soldering2.jpg)
*The matrix of diodes*

## Software

The [official firmware](https://github.com/qmk/qmk_firmware/tree/master/keyboards/lily58) uses [QMK](https://qmk.fm/), since I consider useless the standard display
usage (showing che current keyboard layer (a QMK thing to associate different
keys to the same switch) and the latest typed characters) I [forked it](https://github.com/aciceri/lily58) to add
support for custom text messages using the HID protocol.

![Flashing the firmware](/images/lily58/flashing.jpg)
*Flashing the firmware*

Then I created a simple utility to that sends messages to the keyboard, the
idea was to integrate this utility with Emacs to show things like:

- the minibuffer
- the kill ring
- generic information about the system, like the temperature or the
  CPU/Memory/Disk/Network usage
- currently playing music (I use Mopidy so it's easy with Emacs)

At the end I was able to to show a message from Emacs but I never implemented
all the rest.

## Final result

![Front](/images/lily58/result1.jpg)
*Front*

![Back](/images/lily58/result2.jpg)
*Back*

That's all for now.