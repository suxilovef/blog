---
title: Java-Spring
date: 2025-12-18 20:07:05
description: Java-Spring 框架
tags:
  - Java
  - Spring
categories:
  - Java
top_img: false
cover: /image/post_cover/spring-framework-min.svg
---

**SpringBoot：2.7.18**

**文档版本：GIT**：迭代

## A-00X：SpringApplication

### B-001：前言

SpringBoot执行流程

### B-002：调试

```java
@SpringBootApplication
public class B01Application {
    public static void main(String[] args) {
        ConfigurableApplicationContext context = SpringApplication.run(B01Application.class, args);
    }
}
```

```java
public class B0102Application {
    public static void main(String[] args) throws Exception {
        // 获取BeanDefinition源
        SpringApplication springApplication = new SpringApplication(B0102Application.class);
        springApplication.setSources(Set.of(""));
        // 推断应用程序类型
        Method deduceFromClasspath = WebApplicationType.class.getDeclaredMethod("deduceFromClasspath");
        deduceFromClasspath.setAccessible(true);
        Object invoke = deduceFromClasspath.invoke(null);
        // 添加引导上下文扩展
        springApplication.addBootstrapRegistryInitializer(registry -> {

        });
        // 添加应用主上下文扩展
        springApplication.addInitializers(applicationContext -> {

        });
        // 添加应用事件监听器
        springApplication.addListeners(event -> {

        });
        ConfigurableApplicationContext context = springApplication.run(args);
        
    }
}
```



### B-003：源码解析

#### SpringApplication

```java
public SpringApplication(ResourceLoader resourceLoader, Class<?>... primarySources) {
    this.resourceLoader = resourceLoader;
    Assert.notNull(primarySources, "PrimarySources must not be null");
    // 获取源：主源
    this.primarySources = new LinkedHashSet<>(Arrays.asList(primarySources));
    // 推断应用程序类型：根据类路径下是否存在某些特殊类来判断
    this.webApplicationType = WebApplicationType.deduceFromClasspath();
    // SPI机制加载：BootstrapRegistryInitializer（引导上下文扩展）
    this.bootstrapRegistryInitializers = new ArrayList<>(
        getSpringFactoriesInstances(BootstrapRegistryInitializer.class));
    // SPI机制加载：ApplicationContextInitializer（应用主上下文扩展）
    setInitializers((Collection) getSpringFactoriesInstances(ApplicationContextInitializer.class));
    // SPI机制加载：ApplicationListener（应用事件监听器）
    setListeners((Collection) getSpringFactoriesInstances(ApplicationListener.class));
    // 推断主类
    this.mainApplicationClass = deduceMainApplicationClass();
}
```

#### SpringApplication#run

```java
public ConfigurableApplicationContext run(String... args) {
    long startTime = System.nanoTime();
    // 创建引导上下文：执行引导上下文扩展
    DefaultBootstrapContext bootstrapContext = createBootstrapContext();
    ConfigurableApplicationContext context = null;
    configureHeadlessProperty();
    // 创建：SpringApplicationRunListeners（事件发布器组合）：SPI机制加载SpringApplicationRunListener（事件发布器）
    SpringApplicationRunListeners listeners = getRunListeners(args);
    // 发布：ApplicationStartingEvent
    listeners.starting(bootstrapContext, this.mainApplicationClass);
    try {
        // 解析命令行：ApplicationArguments
        ApplicationArguments applicationArguments = new DefaultApplicationArguments(args);
        // 准备环境
        ConfigurableEnvironment environment = prepareEnvironment(listeners, bootstrapContext, applicationArguments);
        configureIgnoreBeanInfo(environment);
        Banner printedBanner = printBanner(environment);
        // 创建容器
        context = createApplicationContext();
        context.setApplicationStartup(this.applicationStartup);
        // 准备容器：调用ApplicationContextInitializer（对context增强）、加载BeanDefinition
        prepareContext(bootstrapContext, context, environment, listeners, applicationArguments, printedBanner);
        // 刷新容器：
        refreshContext(context);
        afterRefresh(context, applicationArguments);
        Duration timeTakenToStartup = Duration.ofNanos(System.nanoTime() - startTime);
        if (this.logStartupInfo) {
            new StartupInfoLogger(this.mainApplicationClass).logStarted(getApplicationLog(), timeTakenToStartup);
        }
        listeners.started(context, timeTakenToStartup);
        // 执行runner：扩展：业务可能需要：执行两类：ApplicationRunner、CommandLineRunner
        callRunners(context, applicationArguments);
    }
    catch (Throwable ex) {
        handleRunFailure(context, ex, listeners);
        throw new IllegalStateException(ex);
    }
    try {
        Duration timeTakenToReady = Duration.ofNanos(System.nanoTime() - startTime);
        listeners.ready(context, timeTakenToReady);
    }
    catch (Throwable ex) {
        handleRunFailure(context, ex, null);
        throw new IllegalStateException(ex);
    }
    return context;
}
```

### B-004：扩展源码解析

#### C-00X：getRunListeners(args)

##### SpringApplicationRunListeners

```java
class SpringApplicationRunListeners {
    // 类设计思路：
    // 封装发布器集合，便于上层调用；集成ApplicationStartup，性能分析；
    // 单独处理
    private final Log log;
    // 事件发布器集合
    private final List<SpringApplicationRunListener> listeners;

    private final ApplicationStartup applicationStartup;

    SpringApplicationRunListeners(Log log, Collection<? extends SpringApplicationRunListener> listeners,
                                  ApplicationStartup applicationStartup) {
        this.log = log;
        this.listeners = new ArrayList<>(listeners);
        this.applicationStartup = applicationStartup;
    }

    void starting(ConfigurableBootstrapContext bootstrapContext, Class<?> mainApplicationClass) {
        doWithListeners("spring.boot.application.starting", (listener) -> listener.starting(bootstrapContext),
                        (step) -> {
                            if (mainApplicationClass != null) {
                                step.tag("mainApplicationClass", mainApplicationClass.getName());
                            }
                        });
    }
    
    // ......
    private void doWithListeners(String stepName, Consumer<SpringApplicationRunListener> listenerAction) {
        doWithListeners(stepName, listenerAction, null);
    }

    private void doWithListeners(String stepName, Consumer<SpringApplicationRunListener> listenerAction,
                                 Consumer<StartupStep> stepAction) {
        StartupStep step = this.applicationStartup.start(stepName);
        this.listeners.forEach(listenerAction);
        if (stepAction != null) {
            stepAction.accept(step);
        }
        step.end();
    }

}
```

##### EventPublishingRunListener

```java
public class EventPublishingRunListener implements SpringApplicationRunListener, Ordered {

    private final SpringApplication application;

    private final String[] args;
    // 临时事件广播器
    private final SimpleApplicationEventMulticaster initialMulticaster;

    public EventPublishingRunListener(SpringApplication application, String[] args) {
        this.application = application;
        this.args = args;
        this.initialMulticaster = new SimpleApplicationEventMulticaster();
        for (ApplicationListener<?> listener : application.getListeners()) {
            this.initialMulticaster.addApplicationListener(listener);
        }
    }

    @Override
    public int getOrder() {
        return 0;
    }

    @Override
    public void starting(ConfigurableBootstrapContext bootstrapContext) {
        this.initialMulticaster
            .multicastEvent(new ApplicationStartingEvent(bootstrapContext, this.application, this.args));
    }
}
```

##### SimpleApplicationEventMulticaster

临时事件广播器：ApplicationEventMulticaster：TODO

#### C-00X：prepareEnvironment

**关键类**：

**ApplicationEnvironmentPreparedEvent；EnvironmentPostProcessorApplicationListener；EnvironmentPostProcessor；ApplicationServletEnvironment**

```java
private ConfigurableEnvironment prepareEnvironment(SpringApplicationRunListeners listeners,
                                                   DefaultBootstrapContext bootstrapContext, ApplicationArguments applicationArguments) {
    // Create and configure the environment
    // 根据applicationType获取或创建ConfigurableEnvironment：SERVLET 默认ApplicationServletEnvironment
    ConfigurableEnvironment environment = getOrCreateEnvironment();
    // 讲main函数的args封装成SimpleCommandLinePropertySource加入到环境中
    // 根据SimpleCommandLinePropertySource激活响应的配置文件（TODO：这是一个空方法）
    configureEnvironment(environment, applicationArguments.getSourceArgs());
    // 将Spring支持的原生环境Environment适配成SpringBoot的ConfigurationPropertySource，让@ConfigurationProperties可以工作
    ConfigurationPropertySources.attach(environment);
    // 发布基础环境准备好事件:ApplicationEnvironmentPreparedEvent：触发EnvironmentPostProcessorApplicationListener
    // SPI机制加载EnvironmentPostProcessor
    // 执行EnvironmentPostProcessor（扩展点）自定义扩展实现方式：SPI机制
    listeners.environmentPrepared(bootstrapContext, environment);
    // SpringBoot 内置的PropertySource实现，优先级调整至最低；（不分析此方法）
    DefaultPropertiesPropertySource.moveToEnd(environment);
    Assert.state(!environment.containsProperty("spring.main.environment-prefix"),
                 "Environment prefix cannot be set via properties.");
    bindToSpringApplication(environment);
    // 判断用户是否手动设置了Environment；SpringBoot支持自定义Environment
    if (!this.isCustomEnvironment) {
        // 环境类型转换器
        EnvironmentConverter environmentConverter = new EnvironmentConverter(getClassLoader());
        // 根据应用运行场景推断环境类型，并使用环境转换器转换
        environment = environmentConverter.convertEnvironmentIfNecessary(environment, deduceEnvironmentClass());
    }
    // 上一步可能替换掉Environment实例，因此重新适配
    ConfigurationPropertySources.attach(environment);
    return environment;
}
```

#### C-00X：context = createApplicationContext();

**创建应用上下文**

**SERVLET：AnnotationConfigServletWebServerApplicationContext**

```java
public class AnnotationConfigServletWebServerApplicationContext extends ServletWebServerApplicationContext
    implements AnnotationConfigRegistry {
    public AnnotationConfigServletWebServerApplicationContext() {
        this.reader = new AnnotatedBeanDefinitionReader(this);
        this.scanner = new ClassPathBeanDefinitionScanner(this);
    }
}
```

```java
public AnnotatedBeanDefinitionReader(BeanDefinitionRegistry registry, Environment environment) {
    Assert.notNull(registry, "BeanDefinitionRegistry must not be null");
    Assert.notNull(environment, "Environment must not be null");
    this.registry = registry;
    this.conditionEvaluator = new ConditionEvaluator(registry, environment, null);
    // SPI机制注册下面类型Bean定义
    // ConfigurationClassPostProcessor AutowiredAnnotationBeanPostProcessor
    // CommonAnnotationBeanPostProcessor
    // EventListenerMethodProcessor
    // DefaultEventListenerFactory
    AnnotationConfigUtils.registerAnnotationConfigProcessors(this.registry);
}
```



## A-00X：BeanFactoryPostProcessor

## A-00X：**DefaultListableBeanFactory**

```

```

