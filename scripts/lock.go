package main

import (
    "github.com/vova616/screenshot"
    "github.com/BurntSushi/xgb"
    "github.com/BurntSushi/xgb/xinerama"
    "github.com/disintegration/gift"
    "image/png"
    "image"
    "os"
    "strconv"
    "bytes"
    "os/exec"
)

func main() {

    // read lock icon file
    iconChan := make(chan image.Image)
    go func() {
        fdLock, _ := os.Open(os.Args[1])
        defer fdLock.Close()
        pngLock, _ := png.Decode(fdLock)
        iconChan <- image.Image(pngLock)
    }()

    // get primary monitor's position and resolution
    primaryChan := make(chan xinerama.ScreenInfo)
    go func() {
        c, _ := xgb.NewConn()
        xinerama.Init(c)
        reply, _ := xinerama.QueryScreens(c).Reply()
        c.Close()
        primaryChan <- reply.ScreenInfo[0]
    }()

    // take screenshot and pixelate
    raw_root, _ := screenshot.CaptureScreen()
    gift.New(gift.Pixelate(10)).Draw(raw_root, raw_root)

    // wait for lock icon and primary display info
    primary := <-primaryChan
    icon := <-iconChan

    // paste lock onto screenshot
    posX := int(primary.XOrg) + int(primary.Width/2)  - (icon.Bounds().Max.X/2)
    posY := int(primary.YOrg) + int(primary.Height/2) - (icon.Bounds().Max.Y/2)
    gift.New().DrawAt(raw_root, icon, image.Pt(posX, posY), gift.OverOperator)

    // run i3lock with raw byte buffer as stdin
    format := strconv.Itoa(raw_root.Bounds().Dx()) + "x" +
              strconv.Itoa(raw_root.Bounds().Dy()) + ":rgbx"
    i3 := exec.Command( "i3lock", "--raw", format, "-i", "/dev/stdin")
    i3.Stdin = bytes.NewBuffer([]byte(raw_root.Pix))
    i3.Run()
}
