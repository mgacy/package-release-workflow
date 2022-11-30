//
//  String+Utils.swift
//  
//
//  Created by Mathew Gacy on 11/23/22.
//

import Foundation

/// Easily throw generic errors with a text description.
extension String: LocalizedError {
    public var errorDescription: String? {
        return self
    }
}

extension String {
    // Adapted from: https://stackoverflow.com/a/27880748
    func matches(for regEx: NSRegularExpression) -> [String] {
        let results = regEx.matches(
            in: self,
            range: NSRange(startIndex..., in: self))
        
        return results.map {
            String(self[Range($0.range, in: self)!])
        }
    }
}
