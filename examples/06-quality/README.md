# Quality Requirements

Quality requirements (non-functional requirements or NFRs) specify system characteristics like performance, reliability, security, and maintainability. Fastbreak provides a structured way to capture these requirements with metrics, targets, and verification methods.

## Files

| File | Concepts |
|------|----------|
| [01-basic-quality.fbrk](01-basic-quality.fbrk) | `quality` keyword, categories, metrics, targets |
| [02-full-quality.fbrk](02-full-quality.fbrk) | Complete NFR definitions with all fields |

## Key Concepts

### Quality Categories
```fbrk
quality performance "..."   // Speed, latency, throughput
quality reliability "..."   // Uptime, error rates, recovery
quality security "..."      // Authentication, encryption, access
quality scalability "..."   // Load handling, growth capacity
quality usability "..."     // User experience, accessibility
quality maintainability "..." // Code quality, documentation
```

### Basic Structure
```fbrk
quality category "Description" {
    metric: metric_name,
    target: < 100ms,
}
```

### Full Structure
```fbrk
@id("NFR-001")
quality performance "API response time" {
    metric: latency,
    scale: p99,
    target: < 50ms,
    constraint: hard,
    applies_to: action api_endpoint,
    measurement: per_request,
    under_load: {
        concurrent_users: 1000,
        requests_per_second: 500,
    },
    verified_by: [
        test "load_test",
        monitor "datadog_latency",
        benchmark "api_benchmark",
    ],
}
```

## Field Reference

| Field | Purpose | Example |
|-------|---------|---------|
| `metric` | What to measure | `latency`, `error_rate`, `throughput` |
| `scale` | Statistical measure | `mean`, `p50`, `p95`, `p99`, `max` |
| `target` | Desired threshold | `< 100ms`, `>= 99.9%`, `< 0.1%` |
| `constraint` | Hard vs soft requirement | `hard`, `soft` |
| `applies_to` | Scope of requirement | `action name`, `state name` |
| `measurement` | How often measured | `per_request`, `per_minute`, `per_hour` |
| `under_load` | Load conditions | Concurrent users, requests/second |
| `verified_by` | Verification methods | Tests, monitors, benchmarks, audits |
