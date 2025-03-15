{
  description = "WasmEdge Nix Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Function to build WasmEdge with different configurations
        wasmedge =
          {
            version ? "0.14.1",
            cudaSupport ? false,
            noAvxSupport ? false,
          }:

          let
            # OS detection
            os = if pkgs.stdenv.isDarwin then "Darwin" else "Linux";

            # Architecture detection
            arch =
              if pkgs.stdenv.hostPlatform.isAarch64 then
                if pkgs.stdenv.isDarwin then "arm64" else "aarch64"
              else
                "x86_64";

            # Package extension
            pkgExt = if pkgs.stdenv.isDarwin then "darwin_${arch}.tar.gz" else "ubuntu20.04_${arch}.tar.gz";

            # Base URL for packages
            baseUrl = "https://github.com/WasmEdge/WasmEdge/releases/download/${version}";

            # Main package URL and hash (replace with actual hash after first build attempt)
            mainUrl = "${baseUrl}/WasmEdge-${version}-${pkgExt}";
            mainHash = "sha256-ON0Q9OeNM5vpHgw1AQVdTa2b8Iw9xkjgejDfm+otbEo=";

            # CUDA extension
            cudaExt = if cudaSupport then "-cuda" else "";

            # AVX extension
            avxExt = if noAvxSupport then "-noavx" else "";

            # GGML plugin URL and hash (replace with actual hash after first build attempt)
            ggmlUrl = "${baseUrl}/WasmEdge-plugin-wasi_nn-ggml${cudaExt}${avxExt}-${version}-${pkgExt}";
            ggmlHash = "sha256-kvK+63li5tYoG4MZTH+yk/sP41fAuHNnjSbNbwbGMt4=";

            # WASI logging plugin URL and hash (replace with actual hash after first build attempt)
            needsWasiLogging = builtins.substring 0 6 version != "0.14.1";
            wasiLoggingUrl = "${baseUrl}/WasmEdge-plugin-wasi_logging-${version}-${pkgExt}";
            wasiLoggingHash = "0000000000000000000000000000000000000000000000000000";

            # Download sources
            mainSrc = pkgs.fetchurl {
              url = mainUrl;
              sha256 = mainHash;
            };

            ggmlSrc = pkgs.fetchurl {
              url = ggmlUrl;
              sha256 = ggmlHash;
            };

            wasiLoggingSrc =
              if needsWasiLogging then
                pkgs.fetchurl {
                  url = wasiLoggingUrl;
                  sha256 = wasiLoggingHash;
                }
              else
                null;

          in
          pkgs.stdenv.mkDerivation {
            pname = "wasmedge";
            inherit version;

            srcs = [
              mainSrc
              ggmlSrc
            ] ++ pkgs.lib.optional needsWasiLogging wasiLoggingSrc;

            sourceRoot = ".";

            nativeBuildInputs = [ pkgs.gnutar ] ++ pkgs.lib.optional pkgs.stdenv.isLinux pkgs.patchelf;

            unpackPhase = ''
              runHook preUnpack

              mkdir -p main ggml ${if needsWasiLogging then "wasi-logging" else ""}

              echo "Extracting main package..."
              tar -xzf ${mainSrc} -C main

              echo "Extracting GGML plugin..."
              mkdir -p ggml/plugin
              tar -xzf ${ggmlSrc} -C ggml/plugin

              ${
                if needsWasiLogging then
                  ''
                    echo "Extracting WASI logging plugin..."
                    mkdir -p wasi-logging/plugin
                    tar -xzf ${wasiLoggingSrc} -C wasi-logging/plugin
                  ''
                else
                  ""
              }

              runHook postUnpack
            '';

            installPhase = ''
              runHook preInstall

              # Create necessary directories
              mkdir -p $out/{bin,lib,include,plugin}

              # Copy main files
              cp -r main/WasmEdge-${version}-${os}/include/* $out/include/
              cp -r main/WasmEdge-${version}-${os}/bin/* $out/bin/

              if [ -d main/WasmEdge-${version}-${os}/lib64 ]; then
                cp -r main/WasmEdge-${version}-${os}/lib64/* $out/lib/
              else
                cp -r main/WasmEdge-${version}-${os}/lib/* $out/lib/
              fi

              # Copy GGML plugin files
              if [ -d ggml/plugin ]; then
                echo "Installing GGML plugin..."
                cp -r ggml/plugin/* $out/plugin/
              fi

              # Copy WASI logging plugin if needed
              ${
                if needsWasiLogging then
                  ''
                    if [ -d wasi-logging/plugin ]; then
                      echo "Installing WASI logging plugin..."
                      cp -r wasi-logging/plugin/* $out/plugin/
                    fi
                  ''
                else
                  ""
              }

              # Set up correct library paths
              ${
                if pkgs.stdenv.isLinux then
                  ''
                    echo "Patching binaries on Linux..."
                    for bin in $out/bin/*; do
                      if [[ -f "$bin" && -x "$bin" ]]; then
                        patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" "$bin" || true
                        patchelf --set-rpath "$out/lib:$out/plugin" "$bin" || true
                      fi
                    done

                    for lib in $out/lib/*.so* $out/plugin/*.so*; do
                      if [[ -f "$lib" && ! -L "$lib" ]]; then
                        echo "Patching library $lib"
                        patchelf --set-rpath "$out/lib:$out/plugin" "$lib" || true
                      fi
                    done
                  ''
                else
                  ''
                    echo "Setting up macOS library paths..."
                    for bin in $out/bin/*; do
                      if [[ -f "$bin" && -x "$bin" ]]; then
                        install_name_tool -add_rpath "@executable_path/../lib" "$bin" || true
                        install_name_tool -add_rpath "@executable_path/../plugin" "$bin" || true
                      fi
                    done
                  ''
              }

              # Make symbolic links for libraries
              for lib in $out/lib/*.so*; do
                if [[ -f "$lib" && ! -L "$lib" ]]; then
                  base=$(basename "$lib")
                  # Create simpler symlinks for libraries
                  if [[ "$base" =~ \.so\.[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
                    linkname=$(echo "$base" | sed -E 's/\.so\.[0-9]+\.[0-9]+\.[0-9]+/\.so/')
                    (cd $out/lib && ln -sf "$base" "$linkname")
                  fi
                fi
              done

              # Create a wrapper script that properly sets up the environment
              mv $out/bin/wasmedge $out/bin/wasmedge.real
              cat > $out/bin/wasmedge << EOF
              #!/bin/sh
              # Set plugin path
              export WASMEDGE_PLUGIN_PATH="\$WASMEDGE_PLUGIN_PATH:$out/plugin"

              # Pass the current working directory for file access
              # This ensures --dir .:. works correctly with file access
              exec $out/bin/wasmedge.real "\$@"
              EOF
              chmod +x $out/bin/wasmedge

              runHook postInstall
            '';

            meta = with pkgs.lib; {
              description = "WasmEdge is a lightweight, high-performance, and extensible WebAssembly runtime";
              homepage = "https://wasmedge.org/";
              license = licenses.asl20;
              platforms = platforms.unix;
              maintainers = [ ];
            };
          };

        # Create a runnable wrapper for wasmedge with specific version
        makeWasmEdgeRunnable =
          wasmedgePkg:
          pkgs.writeShellScriptBin "run-wasmedge" ''
            #!/bin/sh
            # Set the plugin path explicitly
            export WASMEDGE_PLUGIN_PATH="${wasmedgePkg}/plugin"

            # Run wasmedge with args, preserving the current directory structure
            exec ${wasmedgePkg}/bin/wasmedge "$@"
          '';

      in
      {
        packages = {
          default = self.packages.${system}.wasmedge;

          # Standard WasmEdge
          wasmedge = wasmedge { };

          # WasmEdge with CUDA support
          wasmedge-cuda = wasmedge {
            cudaSupport = true;
          };

          # WasmEdge without AVX support
          wasmedge-noavx = wasmedge {
            noAvxSupport = true;
          };

          # WasmEdge with CUDA and without AVX
          wasmedge-cuda-noavx = wasmedge {
            cudaSupport = true;
            noAvxSupport = true;
          };

          # Runnable wrappers
          run-wasmedge = makeWasmEdgeRunnable self.packages.${system}.wasmedge;
          run-wasmedge-cuda = makeWasmEdgeRunnable self.packages.${system}.wasmedge-cuda;
        };

        apps = {
          default = {
            type = "app";
            program = "${self.packages.${system}.run-wasmedge}/bin/run-wasmedge";
          };
          wasmedge = {
            type = "app";
            program = "${self.packages.${system}.run-wasmedge}/bin/run-wasmedge";
          };
          wasmedge-cuda = {
            type = "app";
            program = "${self.packages.${system}.run-wasmedge-cuda}/bin/run-wasmedge";
          };
        };
      }
    );
}
