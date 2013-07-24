

def format_comhelp (line):
    """
    Takes one line which "ax = description of ax    cx   =  description of cx   bx   =  description of bx"

    and returns:
"ax = description of ax                bx = description of bx\n
cx = description of cx\n"

    """
    esplit = line.split("=")
    
    command_help = {}
    
    for i in xrange(1,len(esplit)):
        cmd = esplit[i-1].split()[-1]
        description = esplit[i].split()[:-1]

        command_help[cmd] = " ".join(description)

    i = 1
    outline = ''
    for cmd in sorted(command_help.keys()):
        outline += format(cmd+" = "+command_help[cmd],'<38')
        
        if i == 2:
            outline += "\n"
            i = 1
        else: i = 2
    return outline,command_help

line = 'ac = addition of a constant           ad = addition of x and y arrays      ag = plot again with same bounds      al = antilog of the points av = average (smooth) the spectrum    bb = blowup the plot in 2 dimensions  bx = blowup in x only                 by = blowup in y only cc = cross-correlate x and y [P]      cd = continuum division ch = chop off ends of array           cl = clear the information screen cr = fix cosmic ray hits              d0 = display the whole x-array dc = make a dispersion curve [P]      de = deconvolve a spectrum df = delete files                     du = dump arrays in (x,y) pairs        dv = division of x by y-array         dx = display x-array                   dy = display y-array                  dz = display z-array                   eb = blowup plot with named bounds    ed = edit bad data points              el = do emission line plots           eq = calculate equivalent widths [P]   fc = flatten continuum, reg. spacing  fl = flatten continuum (file)          ft = fourier transform smoothing      fz = force lower plot bound to zero    gl = bad data point removal           hc = make hardcopy plots               hd = display FITS header lines        he = list commands for SPECTRE         lg = base10 log of points             li = make a Gaussian line ll = change the overplot line list    lp = overplot lines onto current plot    ls = list directory contents          me = merge the x and y arrays          mo = move spectrum to another array   mu = multiply by a constant            no = calculate the S/N of a spectrum  pi = give point value under cursor     pl = repeatedly do "pi"               po = point at the named line           pr = write point values of arrays     px = plot points in x-array            py = plot points in y-array           pz = plot points in z-array            qu = quit SPECTRE                     r0 = remove zeroes from spectra        rb = re-bin for different steps       rd = read a spectrum                   re = replace spectrum file            rm = remove telluric lines [P]         rn = renormalize spectrum             rt = read text file                    sa = save spectrum to new file        sc = set continuum into y-array        si = create a sine wave in x          sp = split display into 2 parts        st = redisplay status screen          su = subtraction of x and y arrays     tc = compute optical depths           ti = give a new FITS object keyword    tl = correct spectrum flux tilt       vn = cancel a radial velocity shift vs = set a desired r.v. shift         vy = apply a radial velocity shift     wv = create an H2O spectrum           xp = expand the number of points xy = exchange x and y arrays          xz = exchange x and z arrays           yz = exchange y and z arrays          ze = zero array ends                   zi = zoom in on a plot                zo = zoom out on a plot                4p = 4 point Reticon normalization    8p = eight point normalization         4n = 4x4 point normalization mc = run commands on multiple files  ?? = help for a command .'


outline,cmd_help = format_comhelp(line)

print outline
