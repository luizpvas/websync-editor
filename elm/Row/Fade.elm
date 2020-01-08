module Row.Fade exposing (tiltLeftSvg, tiltRightSvg, waveSvg)


waveSvg : String
waveSvg =
    """
    <svg class="ws-row-fade" preserveAspectRatio="none" viewBox="0 0 412 14" xmlns="http://www.w3.org/2000/svg">
    <path d="M0 0H412V14C412 14 344 11.3954 309.5 9.43478C275 7.47421 255.5 5.64812 204.5 9.43478C153.5 13.2214 160 14 103 14C46 14 0 4 0 4V0Z" />
    </svg>
    """


tiltRightSvg : String
tiltRightSvg =
    """
    <svg class="ws-row-fade" preserveAspectRatio="none" viewBox="0 0 412 14" xmlns="http://www.w3.org/2000/svg">
    <path d="M0 0H412V14L0 0Z" />
    </svg>
    """


tiltLeftSvg : String
tiltLeftSvg =
    """
    <svg class="ws-row-fade" preserveAspectRatio="none" viewBox="0 0 412 14" xmlns="http://www.w3.org/2000/svg">
    <path d="M412 0H0V14L412 0Z" />
    </svg>
    """
