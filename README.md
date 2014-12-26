Erlang日志系统
可动态控制日志等级:
loglevel:set(Level)
获取当前日志等级:
loglevel:get/0
每次调用后都会重新编译logger,加载module
添加了buffer缓存，每500ms或连续收到100条日志后才刷日志到文件中，优化后可以处理5000/s以上的并发请求。

参考自 ejabberd_logger
