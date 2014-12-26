Erlang日志系统
可动态控制日志等级
loglevel:set(Level)
获取当前日志等级
loglevel:get/0
每次调用后都会重新编译logger,加载module
添加buffer缓存，延时写入文件，可以处理5000/s的并发请求
Erlang OTP通过gen_event行为处理日志,默认日志模块为:error_logger,error_logger:add_report_handler(Mod, Arg)可以修改日志处理方式
参考自 ejabberd_logger
