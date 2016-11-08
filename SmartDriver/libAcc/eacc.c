#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/init.h>

MODULE_LICENSE("GPL");
MODULE_AUTHOR("Daniel Rubio Bonilla");
MODULE_DESCRIPTION("Enable ARM Cycle Counter");

static int __init eacc_init(void) {
  asm ("MCR p15, 0, %0, C9, C14, 0\n\t" :: "r"(1));
  asm ("MCR p15, 0, %0, C9, C14, 2\n\t" :: "r"(0x8000000f));

  printk(KERN_INFO "EACC loaded\n");
  return 0;
}

static void __exit eacc_exit(void) {
  printk(KERN_INFO "EACC unloaded\n");
}

module_init(eacc_init);
module_exit(eacc_exit);
