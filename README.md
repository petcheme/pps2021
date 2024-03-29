
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Modulation of auditory peripersonal space by approaching and receding sources moving in discrete steps

### Ignacio Spiousas, Esteban Lombera, Ramiro O. Vergara and Pablo E. Etchemendy.

This Github repository contains the data and code used in the article
“Modulation of auditory peripersonal space by approaching and receding
sources moving in discrete steps”.

## The study

The region of the space in which we can interact with the outside world
by touching, grasping and grabbing is usually known as the peripersonal
space (PPS). In particular, when the object that we want to reach is
located through audition, we can talk of the auditory peripersonal
space. This extension of the PPS is known to be dynamic and to be
affected by the environment, the inner state of the listener as well as
characteristics of the auditory object, for example if the object is
continuously moving towards or away from the listener. In this paper, we
studied the auditory representation of the peripersonal space for
sources that moved in discrete steps. In each trial, a sound source was
presented to the listener at a given distance and they had to respond
whether the sound source was within their arm’s reach without performing
the action. These reachability judgments involve the representation of
the spatial location of the source and also of the potential actions the
subject can perform, therefore are a good proxy to estimate the
extension of the auditory PPS. In two conditions (simple staircases) the
source moved from trial to trial one step closer or further to the
subject, giving the impression through several consecutive presentations
of a source approaching or receding. In the other two conditions, the
trajectory of the source was not well defined across consecutive
presentations. Our results indicate that reachability judgments are
affected by the previous positions of the sound source. When the source
approaches the listener, it is judged more reachable at the same real
position than when it recedes, suggesting an expansion of the auditory
peripersonal space for approaching sound and extending. When the source
presents a non-defined trajectory (control condition) the reachability
displays an intermediate value. These results are in line with previous
research that showed similar effects for sound sources moving along
continuous trajectories. The shift in the boundaries of the peripersonal
space are interpreted in light of the recent proposals of the function
of the peripersonal space as an interface to the possibilities of action
of the subject against external events.

## The data

The folder data contains the data organized by experiment and
experimental task and include a data.log file. In order to better
explain how the data is organized we will give examples of how the code
works.

## The code

The code can be find in the folder named… well… code.

**Outliers_IQR.R**: Performs the Outliers analysis (it should be run at the beginning).

**Psy_curve.R**: This script process and fit all the psychometric curves and generates *Figure 2*.

**Staircases_example.R**: Generates *Figure 3*.

**Analysis_PSEs.R**: Generates *Figure 4* and *Figure S5.1* (PSEs with and without excluding outliers).

**Analysis_RTs.R**: Generates *Figure 5*.

**Subjects_info.R**: Generates *Figure S1.1* and *Figure S1.2*.

**Staircases_range.R**: Generates *Figure S2.1* on line 157.

**Staircases_first_reversal.R**: Generates *Figure S3.1* on line 95.

**Staircases_first_reversal_RT.R**:  Generates *Figure S4.1* on line 102.

**Analysis_RTs.R**: Generates *Figure S6.1* (individual fits of RT) on section 2.

## Questions or comments?

If you have any additional questions or comments about the data or code
feel free to contact Ignacio Spiousas (ispiousas (at) udesa.edu.ar) or
Pablo E. Etchemendy (petcheme (at) untref.edu.ar).

## Preprint version of the article

https://psyarxiv.com/9nfqz/
