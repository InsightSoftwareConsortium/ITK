/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:     H5ESlist.c
 *              Nov  7 2020
 *	            Quincey Koziol
 *
 * Purpose:     Operations on "event lists" for managing asynchronous
 *                      operations.
 *
 *                      Please see the asynchronous I/O RFC document
 *                      for a full description of how they work, etc.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5ESmodule.h" /* This source code file is part of the H5ES module */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions			 */
#include "H5Eprivate.h"  /* Error handling		  	 */
#include "H5ESpkg.h"     /* Event Sets                           */
#include "H5FLprivate.h" /* Free Lists                           */

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Package Typedefs */
/********************/

/********************/
/* Local Prototypes */
/********************/

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/*-------------------------------------------------------------------------
 * Function:    H5ES__list_insert
 *
 * Purpose:     Append an event into an event list
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:  Quincey Koziol
 *              Saturday, November 7, 2020
 *
 *-------------------------------------------------------------------------
 */
void
H5ES__list_append(H5ES_event_list_t *el, H5ES_event_t *ev)
{
    FUNC_ENTER_PACKAGE_NOERR

    /* Sanity check */
    HDassert(el);
    HDassert(ev);

    /* Append event onto the event list */
    if (NULL == el->tail)
        el->head = el->tail = ev;
    else {
        ev->prev       = el->tail;
        el->tail->next = ev;
        el->tail       = ev;
    } /* end else */

    /* Increment the # of events in list */
    el->count++;

    FUNC_LEAVE_NOAPI_VOID
} /* end H5ES__list_append() */

/*-------------------------------------------------------------------------
 * Function:    H5ES__list_count
 *
 * Purpose:     Retrieve # of events in an event list
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:  Quincey Koziol
 *              Saturday, November 7, 2020
 *
 *-------------------------------------------------------------------------
 */
H5_ATTR_PURE size_t
H5ES__list_count(const H5ES_event_list_t *el)
{
    FUNC_ENTER_PACKAGE_NOERR

    /* Sanity check */
    HDassert(el);

    FUNC_LEAVE_NOAPI(el->count)
} /* end H5ES__list_count() */

/*-------------------------------------------------------------------------
 * Function:    H5ES__list_iterate
 *
 * Purpose:     Iterate over events in a list, calling callback for
 *              each event.
 *
 * Note:        Iteration is safe for deleting the current event.  Modifying
 *              the list in other ways is likely unsafe.
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:  Quincey Koziol
 *              Saturday, November 7, 2020
 *
 *-------------------------------------------------------------------------
 */
int
H5ES__list_iterate(H5ES_event_list_t *el, H5ES_list_iter_func_t cb, void *ctx)
{
    H5ES_event_t *ev;                       /* Event in list */
    int           ret_value = H5_ITER_CONT; /* Return value */

    FUNC_ENTER_PACKAGE_NOERR

    /* Sanity check */
    HDassert(el);
    HDassert(cb);

    /* Iterate over events in list */
    ev = el->head;
    while (ev) {
        H5ES_event_t *tmp; /* Temporary event */

        /* Get pointer to next node, so it's safe if this one is removed */
        tmp = ev->next;

        /* Perform iterator callback */
        if ((ret_value = (*cb)(ev, ctx)) != H5_ITER_CONT) {
            if (ret_value < 0)
                HERROR(H5E_EVENTSET, H5E_CANTNEXT, "iteration operator failed");
            break;
        } /* end if */

        /* Advance to next node */
        ev = tmp;
    } /* end while */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5ES__list_iterate() */

/*-------------------------------------------------------------------------
 * Function:    H5ES__list_remove
 *
 * Purpose:     Remove an event from an event list
 *
 * Return:      SUCCEED / FAIL
 *
 * Programmer:  Houjun Tang
 *              Thursday, July 30, 2020
 *
 *-------------------------------------------------------------------------
 */
void
H5ES__list_remove(H5ES_event_list_t *el, const H5ES_event_t *ev)
{
    FUNC_ENTER_PACKAGE_NOERR

    /* Sanity check */
    HDassert(el);
    HDassert(el->head);
    HDassert(ev);

    /* Stitch event out of list */
    if (ev == el->head)
        el->head = ev->next;
    if (NULL != ev->next)
        ev->next->prev = ev->prev;
    if (NULL != ev->prev)
        ev->prev->next = ev->next;
    if (NULL == el->head)
        el->tail = NULL;

    /* Decrement the # of events in list */
    el->count--;

    FUNC_LEAVE_NOAPI_VOID
} /* end H5ES__list_remove() */
