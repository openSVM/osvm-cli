//! Unit tests for SSH deployment

#[cfg(test)]
mod tests {
    use {
        crate::utils::ssh_deploy::{
            services::{create_binary_service_content, create_docker_service_content},
            types::{AuthMethod, DeploymentConfig, NetworkType, ServerConfig},
            validators::{
                get_required_resources, validate_cpu_cores, validate_disk_space, validate_memory,
                validate_system_requirements,
            },
        },
        std::collections::HashMap,
        std::str::FromStr,
    };

    #[test]
    fn test_network_type_display() {
        assert_eq!(NetworkType::Mainnet.to_string(), "mainnet");
        assert_eq!(NetworkType::Testnet.to_string(), "testnet");
        assert_eq!(NetworkType::Devnet.to_string(), "devnet");
    }

    #[test]
    fn test_network_type_from_str() {
        assert_eq!(
            NetworkType::from_str("mainnet").unwrap(),
            NetworkType::Mainnet
        );
        assert_eq!(
            NetworkType::from_str("testnet").unwrap(),
            NetworkType::Testnet
        );
        assert_eq!(
            NetworkType::from_str("devnet").unwrap(),
            NetworkType::Devnet
        );
        assert!(NetworkType::from_str("invalid").is_err());
    }

    #[test]
    fn test_server_config_from_connection_string() {
        // Test valid connection string without port
        let config = ServerConfig::from_connection_string("user@host").unwrap();
        assert_eq!(config.host, "host");
        assert_eq!(config.port, 22);
        match &config.auth {
            AuthMethod::Key {
                username,
                key_path,
                passphrase,
            } => {
                assert_eq!(username, "user");
                assert_eq!(key_path, "~/.ssh/id_rsa");
                assert!(passphrase.is_none());
            }
            _ => panic!("Expected Key authentication method"),
        }
        assert_eq!(config.install_dir, "/opt/osvm");

        // Test valid connection string with port
        let config = ServerConfig::from_connection_string("user@host:2222").unwrap();
        assert_eq!(config.host, "host");
        assert_eq!(config.port, 2222);

        // Test invalid connection string
        assert!(ServerConfig::from_connection_string("invalid").is_err());
    }

    #[test]
    fn test_get_required_resources() {
        // Test valid combinations
        assert_eq!(
            get_required_resources("solana", "validator").unwrap(),
            (12, 128, 2048)
        );
        assert_eq!(
            get_required_resources("solana", "rpc").unwrap(),
            (16, 256, 4096)
        );
        assert_eq!(
            get_required_resources("sonic", "validator").unwrap(),
            (8, 32, 1024)
        );
        assert_eq!(
            get_required_resources("sonic", "rpc").unwrap(),
            (16, 64, 2048)
        );
        assert_eq!(
            get_required_resources("sui", "validator").unwrap(),
            (8, 32, 1024)
        );
        assert_eq!(
            get_required_resources("sui", "rpc").unwrap(),
            (16, 64, 2048)
        );
        assert_eq!(
            get_required_resources("aptos", "validator").unwrap(),
            (8, 32, 1024)
        );
        assert_eq!(
            get_required_resources("aptos", "rpc").unwrap(),
            (16, 64, 2048)
        );

        // Test invalid combinations
        assert!(get_required_resources("invalid", "validator").is_err());
        assert!(get_required_resources("solana", "invalid").is_err());
    }

    #[test]
    fn test_validate_cpu_cores() {
        let mut system_info = HashMap::new();

        // Test sufficient CPU cores
        system_info.insert("cpu_cores".to_string(), "16".to_string());
        assert!(validate_cpu_cores(&system_info, 12).is_ok());

        // Test insufficient CPU cores
        system_info.insert("cpu_cores".to_string(), "8".to_string());
        assert!(validate_cpu_cores(&system_info, 12).is_err());

        // Test missing CPU cores
        system_info.remove("cpu_cores");
        assert!(validate_cpu_cores(&system_info, 12).is_err());

        // Test invalid CPU cores
        system_info.insert("cpu_cores".to_string(), "invalid".to_string());
        assert!(validate_cpu_cores(&system_info, 12).is_err());
    }

    #[test]
    fn test_validate_memory() {
        let mut system_info = HashMap::new();

        // Test sufficient memory
        system_info.insert("memory_gb".to_string(), "256".to_string());
        assert!(validate_memory(&system_info, 128).is_ok());

        // Test insufficient memory
        system_info.insert("memory_gb".to_string(), "64".to_string());
        assert!(validate_memory(&system_info, 128).is_err());

        // Test missing memory
        system_info.remove("memory_gb");
        assert!(validate_memory(&system_info, 128).is_err());

        // Test invalid memory
        system_info.insert("memory_gb".to_string(), "invalid".to_string());
        assert!(validate_memory(&system_info, 128).is_err());
    }

    #[test]
    fn test_validate_disk_space() {
        let mut system_info = HashMap::new();

        // Test sufficient disk space (GB)
        system_info.insert("disk_available".to_string(), "4096G".to_string());
        assert!(validate_disk_space(&system_info, 2048).is_ok());

        // Test sufficient disk space (TB)
        system_info.insert("disk_available".to_string(), "4T".to_string());
        assert!(validate_disk_space(&system_info, 2048).is_ok());

        // Test insufficient disk space
        system_info.insert("disk_available".to_string(), "1024G".to_string());
        assert!(validate_disk_space(&system_info, 2048).is_err());

        // Test missing disk space
        system_info.remove("disk_available");
        assert!(validate_disk_space(&system_info, 2048).is_err());

        // Test invalid disk space
        system_info.insert("disk_available".to_string(), "invalid".to_string());
        assert!(validate_disk_space(&system_info, 2048).is_err());
    }

    #[test]
    fn test_validate_system_requirements() {
        let mut system_info = HashMap::new();
        system_info.insert("cpu_cores".to_string(), "16".to_string());
        system_info.insert("memory_gb".to_string(), "256".to_string());
        system_info.insert("disk_available".to_string(), "4096G".to_string());

        let deployment_config = DeploymentConfig {
            svm_type: "solana".to_string(),
            node_type: "rpc".to_string(),
            network: NetworkType::Mainnet,
            node_name: "test-node".to_string(),
            rpc_url: None,
            additional_params: HashMap::new(),
            version: None,
            client_type: None,
            hot_swap_enabled: false,
            metrics_config: None,
            disk_config: None,
        };

        // Test valid system requirements
        assert!(validate_system_requirements(&system_info, &deployment_config).is_ok());

        // Test invalid system requirements (insufficient CPU)
        system_info.insert("cpu_cores".to_string(), "8".to_string());
        assert!(validate_system_requirements(&system_info, &deployment_config).is_err());

        // Reset CPU and test invalid system requirements (insufficient memory)
        system_info.insert("cpu_cores".to_string(), "16".to_string());
        system_info.insert("memory_gb".to_string(), "128".to_string());
        assert!(validate_system_requirements(&system_info, &deployment_config).is_err());

        // Reset memory and test invalid system requirements (insufficient disk)
        system_info.insert("memory_gb".to_string(), "256".to_string());
        system_info.insert("disk_available".to_string(), "2048G".to_string());
        assert!(validate_system_requirements(&system_info, &deployment_config).is_err());
    }

    #[test]
    fn test_create_docker_service_content() {
        let service_content =
            create_docker_service_content("test-service", "/opt/test", "Test Service");

        assert!(service_content.contains("Description=Test Service"));
        assert!(service_content.contains("WorkingDirectory=/opt/test"));
        assert!(service_content.contains("ExecStart=/usr/local/bin/docker-compose up"));
        assert!(service_content.contains("ExecStop=/usr/local/bin/docker-compose down"));
    }

    #[test]
    fn test_create_binary_service_content() {
        let args = vec!["--arg1 value1", "--arg2 value2"];

        let service_content = create_binary_service_content(
            "/usr/bin/test-binary",
            &args,
            "/opt/test",
            "Test Service",
        );

        assert!(service_content.contains("Description=Test Service"));
        assert!(service_content.contains("WorkingDirectory=/opt/test"));
        assert!(service_content.contains("ExecStart=/usr/bin/test-binary"));
        assert!(service_content.contains("--arg1 value1"));
        assert!(service_content.contains("--arg2 value2"));
    }
}