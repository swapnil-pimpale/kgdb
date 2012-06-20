/*
 * gdbmod.c	A helper module for an external gdb stub.
 *
 * Copyright (C) 2002 Ganesh Varadarajan <ganesh@veritas.com>
 *
 *     This program is free software; you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation; either version 2 of the License, or
 *     (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include <linux/config.h>
#include <linux/module.h>
#include <linux/sched.h>
#include <linux/init.h>

#define GDBMOD_DATASZ		(100 * 1024)

#define GDBMOD_THREADINFO	1
#define GDBMOD_TEST		2

#define GPR6	0
#define GPR7	1
#define GPR8	2
#define GPR9	3
#define GPR10	4
#define GPR11	5
#define GPR12	6
#define GPR13	7
#define GPR14	8
#define GPR15	9

/*
 * Per-thread data which we return. Keep it as small as possible. Currently
 * weighs in at 64 bytes.
 */

struct thread_data {
	unsigned long	pid;
	char		comm[16];	/* Name */
	int		processor;	/* processor field from struct
					   task_struct. MSB set if actually
					   running */
	unsigned long	gprs[10];	/* GPRs 6 to 15. The first 5 are
					   call-clobbered anyway. */
};

/*
 * All data to be returned is stuffed here.
 */

unsigned char		gdbmod_data[GDBMOD_DATASZ] __attribute__ ((aligned (8)));

/*
 * Private stack for use by functions in this module.
 */

unsigned long		gdbmod_stack[PAGE_SIZE / sizeof(unsigned long)] __attribute__ ((aligned (8)));

/*
 * This function only exists to provide a convenient place to put a breakpoint
 * when everything's done. This is the only way we can "return" to CP, since
 * we use our own stack.
 */

void gdbmod_return(void *arg)
{

	sprintf(gdbmod_data, "Should never get here! %lx", (unsigned long)arg);
	return;
}

/*
 * Walk the task list and stuff the relevant details into the gdbmod_data
 * array.
 */

void *gdbmod_threadinfo(void *arg)
{
	struct task_struct	*p;
	struct thread_struct	*t;
	struct thread_data	*td = (struct thread_data *)gdbmod_data;
	struct thread_data	*tdp;
	int			i = 1;
	unsigned long		*sp;

	for_each_task(p) {
		tdp = td + i;
		tdp->pid = p->pid;
		strncpy(tdp->comm, p->comm, 16);
		tdp->processor = p->processor;
		if (p->has_cpu) {
			tdp->processor |= 0x80000000;
			goto next;
		}

		/*
		 * Except those tasks which are currently running, all
		 * others will show up in "info threads" as within
		 * the function schedule(). This being somewhat less
		 * than useful, we walk the stack one frame down and
		 * return that instead. Of course now we see many
		 * stuck in schedule_timeout() instead. Maybe we should
		 * walk a further frame down if it's schedule_timeout().
		 */

		tdp->gprs[GPR15] = ((unsigned long *)p->thread.ksp)[0];
		sp = (unsigned long *)tdp->gprs[GPR15];
		tdp->gprs[GPR6] = sp[6];
		tdp->gprs[GPR7] = sp[7];
		tdp->gprs[GPR8] = sp[8];
		tdp->gprs[GPR9] = sp[9];
		tdp->gprs[GPR10] = sp[10];
		tdp->gprs[GPR11] = sp[11];
		tdp->gprs[GPR12] = sp[12];
		tdp->gprs[GPR13] = sp[13];
		tdp->gprs[GPR14] = sp[14];

		/*
		 * Note that the address in r14 will be the return address
		 * from schedule(). The stub will feed the contents of r14 to
		 * gdb as the address part of the PSW.
		 * r0-r5 are call-clobbered, so don't bother returning them.
		 * Space is of the essence.
		 */

next:
		i++;
		if (i >= GDBMOD_DATASZ / sizeof (struct thread_data)) {
			break;
		}
	}
	td[0].pid = i - 1;	/* First record contains number of threads */
	td[0].processor = (unsigned long)arg; /* arg is a cookie */
	return (void *)gdbmod_data;
}

/*
 * This function has no caller. It is directly invoked by the stub by setting
 * the PSW to this address, supplying arguments in r2 and r3, and setting
 * r15 (stack pointer) to gdbmod_stack + 3k. gdbmod_dispatch() will
 * (uselessly) save the caller registers above gdbmod_stack + 3k. Actually
 * only 96 bytes of space are necessary above the stack pointer, but what
 * the heck...
 */

void gdbmod_dispatch(int cmd, void *arg)
{
	void	*ret;

	switch (cmd) {
		case GDBMOD_THREADINFO:
			ret = gdbmod_threadinfo(arg);
			break;
		case GDBMOD_TEST:
			sprintf(gdbmod_data, "Testing - received %lx", (unsigned long)arg);
			ret = (void *)gdbmod_data;
			break;
		default:
			ret = NULL;
			break;
	}
	
	/*
	 * Back to CP we go, one way or another. If the stub has for some reason
	 * not setup the breakpoint on gdbmod_return(), the guest machine will
	 * likely die a flaming death as we return to nowhere.
	 */

	gdbmod_return(ret);
}

int gdbmod_init(void)
{

	return 0;
}

void gdbmod_exit(void)
{
}

module_init(gdbmod_init);
module_exit(gdbmod_exit);
MODULE_LICENSE("GPL");
